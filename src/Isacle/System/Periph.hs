-- NB: NoImplicitPrelude is active from cabal common-options.
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
module Isacle.System.Periph
    ( -- * Signal operations record
      PeriphOps(..)
    , nullOps
      -- * Peripheral definition monad
    , PeriphDef
    , runPeriphDef
      -- * Signal-level register operations
    , onWrite
    , onWriteStrobe
    , onRead
      -- * Register / field declarations (metadata)
    , field
    , field8
    , register
      -- * Spec types
    , PeriphSpec(..)
    , FieldSpec(..)
    , BitField(..)
    , RegWidth(..)
    , RegAccess(..)
    , specSize
      -- * BitField helpers
    , bitF
    , bitsF
      -- * Physical I/O typeclass
    , HasPhysIO(..)
    ) where

import Prelude
import Data.Kind (Type)
import Data.Word (Word8, Word32)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (Nat)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader

import Isacle.System.Spec (NullSig(..))

-- ---------------------------------------------------------------------------
-- Register access and width
-- ---------------------------------------------------------------------------

data RegAccess = ReadOnly | ReadWrite | WriteOnly deriving (Show, Eq)

data RegWidth = RW8 | RW16 | RW32 deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Field specification (two-level: register → bit-fields)
-- ---------------------------------------------------------------------------

-- | A named bit range inside a register.
data BitField = BitField
    { bfLoBit  :: Word8
    , bfHiBit  :: Word8
    , bfAccess :: RegAccess
    , bfName   :: String
    , bfDesc   :: String
    } deriving (Show)

-- | A memory-mapped register at a byte offset.
data FieldSpec = FieldSpec
    { fieldOffset    :: Word8
    , fieldWidth     :: RegWidth
    , fieldAccess    :: RegAccess
    , fieldName      :: String
    , fieldDesc      :: String
    , fieldBitFields :: [BitField]
    } deriving (Show)

-- | Structural description of a peripheral (analysis path only).
newtype PeriphSpec = PeriphSpec
    { psFields :: [FieldSpec]
    } deriving (Show)

-- | Address window size in bytes, derived from the highest declared field.
specSize :: PeriphSpec -> Word32
specSize (PeriphSpec []) = 0
specSize (PeriphSpec fs) = maximum
    [ fromIntegral (fieldOffset f) + widthBytes (fieldWidth f) | f <- fs ]
  where
    widthBytes RW8  = 1
    widthBytes RW16 = 2
    widthBytes RW32 = 4

-- ---------------------------------------------------------------------------
-- PeriphOps: injected register-creation operations
-- ---------------------------------------------------------------------------

-- | Operations record injected into the 'PeriphDef' monad.
--
-- Using a record rather than a typeclass avoids the GHC restriction that
-- implicit-parameter constraints (such as 'HiddenClockResetEnable') cannot
-- appear in instance heads.
--
-- Two values exist:
--
--   * 'nullOps' — spec / documentation interpreter; all operations are no-ops.
--   * @'synthOps'@ (in "Isacle.System.Circuit") — Clash synthesis; 'sigReg'
--     compiles to @regEn@.
newtype PeriphOps (sig :: Type -> Type) dat = PeriphOps
    { -- | Create a clocked register.
      -- @sigReg initVal writeEnable writeData@ → current register value.
      sigReg :: dat -> sig Bool -> sig dat -> sig dat
    }

-- | Spec / documentation ops: register creation is a no-op.
nullOps :: PeriphOps NullSig dat
nullOps = PeriphOps { sigReg = \_ _ _ -> NullSig }

-- ---------------------------------------------------------------------------
-- PeriphDef: peripheral definition monad
-- ---------------------------------------------------------------------------

type WriteBus sig dat = sig (Maybe (Word8, dat))

data PeriphEnv sig dat = PeriphEnv
    { peOps    :: PeriphOps sig dat
    , peWrite  :: WriteBus sig dat
    , peRdAddr :: sig (Maybe Word8)   -- ^ current read address (offset from base)
    }

data PeriphAccum sig dat = PeriphAccum
    { paFields :: [FieldSpec]
    , paRdData :: sig dat             -- ^ accumulated read-mux output signal
    }

emptyAccum :: (Applicative sig, Num dat) => PeriphAccum sig dat
emptyAccum = PeriphAccum [] (pure 0)

-- | Peripheral description monad.
--
-- @p@   — phantom peripheral kind tag (e.g. @GPIO@, @UART@).
-- @sig@ — signal family; 'NullSig' for spec, @Signal dom@ for synthesis.
-- @dat@ — bus data type (e.g. @BitVector 8@).
-- @a@   — return type (the peripheral's physical output signals).
--
-- A single @do@-block mixes structural declarations ('field', 'register') and
-- signal-level circuit operations ('onWrite', 'onRead').
newtype PeriphDef (p :: Type) (sig :: Type -> Type) dat a = PeriphDef
    { unPeriphDef :: ReaderT (PeriphEnv sig dat) (State (PeriphAccum sig dat)) a }
    deriving newtype (Functor, Applicative, Monad)

-- | Run a peripheral definition with the given ops, write bus, and read address.
--
-- Returns the physical outputs, the assembled read-data signal (a chain of
-- combinational mux operations, not a list), and the structural 'PeriphSpec'.
runPeriphDef
    :: (Applicative sig, Num dat)
    => PeriphOps sig dat
    -> WriteBus sig dat         -- ^ write bus (offset-relative)
    -> sig (Maybe Word8)        -- ^ read address (offset-relative)
    -> PeriphDef p sig dat a
    -> (a, sig dat, PeriphSpec)
runPeriphDef ops wrBus rdAddr def =
    let env      = PeriphEnv { peOps = ops, peWrite = wrBus, peRdAddr = rdAddr }
        (a, acc) = runState (runReaderT (unPeriphDef def) env) emptyAccum
    in (a, paRdData acc, PeriphSpec (reverse (paFields acc)))

-- ---------------------------------------------------------------------------
-- Signal-level circuit operations
-- ---------------------------------------------------------------------------

-- | Declare a registered output at @offset@.
--
-- In the synthesis interpreter the returned signal updates on every write to
-- @base + offset@ from the bus.  Write-first semantics: reading the register
-- in the same cycle as a write returns the newly written value (combinational
-- bypass), not the value clocked in on the previous edge.
--
-- In the spec interpreter it returns 'NullSig'.
onWrite
    :: Applicative sig
    => Word8         -- ^ byte offset from peripheral base
    -> dat           -- ^ initial / reset value
    -> PeriphDef p sig dat (sig dat)
onWrite off initVal = PeriphDef $ do
    PeriphEnv { peOps = ops, peWrite = wr } <- ask
    let wen  = fmap (maybe False (\(a, _) -> a == off)) wr
        wdat = fmap (maybe initVal snd) wr
        reg  = sigReg ops initVal wen wdat
    pure ((\w d r -> if w then d else r) <$> wen <*> wdat <*> reg)

-- | Like 'onWrite' but also returns a write-strobe: a signal that is @True@
-- for exactly the cycle on which the CPU writes to @offset@.
--
-- Useful for peripherals (e.g. UART) that need to react to the write event
-- rather than just reading the current register value.
onWriteStrobe
    :: Applicative sig
    => Word8
    -> dat
    -> PeriphDef p sig dat (sig dat, sig Bool)
onWriteStrobe off initVal = PeriphDef $ do
    PeriphEnv { peOps = ops, peWrite = wr } <- ask
    let wen  = fmap (maybe False (\(a, _) -> a == off)) wr
        wdat = fmap (maybe initVal snd) wr
        reg  = sigReg ops initVal wen wdat
        out  = (\w d r -> if w then d else r) <$> wen <*> wdat <*> reg
    pure (out, wen)

-- | Wire @sig@ into the read-data mux at @offset@.
--
-- Each call extends the accumulated read-data signal with one combinational
-- select: if the read address equals @offset@, return @sig@, otherwise pass
-- through the previously accumulated value.  No list is used; the result is
-- a direct chain of @\<$\>@\/@\<*\>@ that Clash can synthesize.
onRead
    :: Applicative sig
    => Word8
    -> sig dat
    -> PeriphDef p sig dat ()
onRead off sig = PeriphDef $ do
    PeriphEnv { peRdAddr = rd } <- ask
    lift $ modify $ \acc ->
        let prev      = paRdData acc
            newRdData = (\mrd s r -> case mrd of
                            Just a | a == off -> s
                            _                 -> r)
                        <$> rd <*> sig <*> prev
        in acc { paRdData = newRdData }

-- ---------------------------------------------------------------------------
-- Structural metadata declarations
-- ---------------------------------------------------------------------------

-- | Declare a monolithic register at @offset@.
field :: RegWidth -> RegAccess -> Word8 -> String -> String
      -> PeriphDef p sig dat ()
field width acc off name desc = PeriphDef $ lift $ modify $ \a ->
    a { paFields = paFields a ++ [FieldSpec off width acc name desc []] }

-- | Shorthand for an 8-bit monolithic register.
field8 :: RegAccess -> Word8 -> String -> String -> PeriphDef p sig dat ()
field8 = field RW8

-- | Declare a register whose word is split into named bit-fields.
register :: RegWidth -> Word8 -> String -> String -> [BitField]
         -> PeriphDef p sig dat ()
register width off name desc bfs = PeriphDef $ lift $ modify $ \a ->
    a { paFields = paFields a ++ [FieldSpec off width ReadWrite name desc bfs] }

-- | A single-bit sub-field inside a register.
bitF :: RegAccess -> Word8 -> String -> String -> BitField
bitF acc b = BitField b b acc

-- | A multi-bit sub-field inside a register.
bitsF :: RegAccess -> Word8 -> Word8 -> String -> String -> BitField
bitsF acc lo hi = BitField lo hi acc

-- ---------------------------------------------------------------------------
-- Physical I/O typeclass
-- ---------------------------------------------------------------------------

-- | Associates a peripheral kind @p@ with its physical signal types.
class HasPhysIO (p :: Type) where
    -- | Byte size of the peripheral's address window.  Used by synthesis to
    -- allocate address decode ranges without running the PeriphDef monad at
    -- circuit-normalization time (which Clash cannot evaluate).
    type PeriphSize p :: Nat
    type PeriphSize p = 256
    type PhysInputs  p (sig :: Type -> Type) :: Type
    type PhysOutputs p (sig :: Type -> Type) :: Type
    nullOutputs :: Proxy p -> PhysOutputs p NullSig
