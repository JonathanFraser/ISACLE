-- NB: NoImplicitPrelude is active from cabal common-options.
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Isacle.System.Builder
    ( -- * System-level DSL
      SystemDSL(..)
    , simpleHarvardCore
      -- * Proxy-free convenience wrappers
    , mkRam
    , mkRom
    , externalIn
    , externalOut
      -- * CPU typeclass
    , HarvardCPU(..)
      -- * Interrupt controller
    , IrqDef(..)
    , noIrqs
    , simpleIrqVec
      -- * Spec interpreter
    , SpecWriter
    , runSpecWriter
    ) where

import Prelude
import Data.Proxy (Proxy(..))
import Data.Word (Word32)
import Clash.Prelude (Signal, KnownDomain, NFDataX, Vec, KnownNat,
                      BitVector, Unsigned, Div, type (<=), liftA2,
                      Clock, Reset, Enable)
import GHC.TypeLits (KnownSymbol, Nat, symbolVal, natVal)
import Control.Monad.Trans.State.Strict

import Isacle.System.Spec
import Isacle.System.Periph (PeriphDef, HasPhysIO(..), runPeriphDef, nullOps, specSize)
import Isacle.System.BusDef (BusDef, Bus(..), BusPeriph(..), BusMaster(..), periph, ramSegment, romBlock, runBusDef)
import Isacle.System.BusArch (BusArch)

-- ---------------------------------------------------------------------------
-- Interrupt controller
-- ---------------------------------------------------------------------------

-- | Opaque interrupt-controller output.  Different IC implementations
-- produce this token; 'cpuSynthWire' consumes it.
--
-- * 'noIrqs'       — no interrupts wired
-- * 'simpleIrqVec' — combinational priority encoder
-- * 'mkPicCtrl'    — (future) programmable IC with data-bus registers
newtype IrqDef sig = IrqDef { runIrqDef :: sig (Maybe (Unsigned 16)) }

-- | No interrupt controller: IRQ line is permanently deasserted.
noIrqs :: Applicative sig => IrqDef sig
noIrqs = IrqDef (pure Nothing)

-- | Simple combinational priority encoder.
-- Sources are in priority order: head = highest priority.
-- The 'Unsigned 16' is the ISA-specific vector word address (e.g. AVR word address).
simpleIrqVec :: Applicative sig => [(sig Bool, Unsigned 16)] -> IrqDef sig
simpleIrqVec sources = IrqDef $ foldr step (pure Nothing) sources
  where
    step (req, vec) prev = liftA2 (\r p -> if r then Just vec else p) req prev

-- ---------------------------------------------------------------------------
-- CPU association typeclass
-- ---------------------------------------------------------------------------

-- | Associates a CPU type with its code-fetch width and synthesis wiring.
--
-- Instances must implement 'cpuSynthWire' using explicit Clash primitives
-- from "Clash.Explicit.Prelude" (taking 'Clock', 'Reset', 'Enable' explicitly)
-- so that no 'HiddenClockResetEnable' constraint is required in this class.
--
-- The default implementation is a no-op that drives the bus with 'pure Nothing'
-- and holds the program counter at 0, suitable for spec-only CPUs.
class KnownNat (FetchWidth cpu) => HarvardCPU cpu where
    type FetchWidth cpu :: Nat

    -- | Wire up the CPU as a bus master, returning the signals it drives.
    --
    -- All Clash register\/RAM primitives must be called using their explicit
    -- forms (e.g. @Clash.Explicit.register clk rst en@) so that the
    -- 'Clock', 'Reset', and 'Enable' can be forwarded from 'runSynth'
    -- without storing them as rank-2 functions.
    --
    -- The default returns a permanently-idle master (all outputs @Nothing@
    -- or @0@), useful for spec-only CPU stubs.
    cpuSynthWire
        :: KnownDomain dom
        => cpu
        -> Clock dom
        -> Reset dom
        -> Enable dom
        -> Signal dom (Maybe (Unsigned 16))   -- ^ interrupt vector
        -> Signal dom (BitVector 16)           -- ^ code ROM data
        -> Signal dom (BitVector 8)            -- ^ data bus read data
        -> BusMaster (Signal dom) (BitVector 8)
    cpuSynthWire _ _ _ _ _ _ _ = BusMaster
        { bmRdAddr   = pure Nothing
        , bmWrBus    = pure Nothing
        , bmCodeAddr = pure 0
        }

-- ---------------------------------------------------------------------------
-- SystemDSL typeclass
-- ---------------------------------------------------------------------------

-- | System-level DSL: the layer where external pins meet peripheral circuits.
--
-- @m@   — the DSL monad (e.g. 'SpecWriter', @Synth dom@).
-- @sig@ — signal functor for this interpreter.
--           * 'SpecWriter':  @sig = NullSig@
--           * @Synth dom@:   @sig = Signal dom@
-- @dat@ — bus data-word type (e.g. @BitVector 8@).
--
-- The functional dependency @m -> sig dat@ ensures that the monad uniquely
-- determines the signal and data types, allowing GHC and Clash to resolve
-- them without type family reduction.
class (Monad m, Applicative sig) => SystemDSL m sig dat | m -> sig dat where

    -- | Declare a named external input port (optional).
    externalInP
        :: KnownSymbol name
        => Proxy name
        -> m (sig t)

    -- | Declare a named external output port (optional).
    externalOutP
        :: KnownSymbol name
        => Proxy name
        -> sig t
        -> m ()

    -- | Instantiate a peripheral and return a 'BusDef' token plus physical
    -- output signals.
    mkPeriph
        :: (HasPhysIO p, KnownNat (PeriphSize p))
        => PeriphDef p sig dat (PhysOutputs p sig)
        -> m (BusDef (), PhysOutputs p sig)

    -- | Instantiate a synchronous block RAM of @n@ bytes.
    mkRamP :: KnownNat n => Proxy n -> m (BusDef ())

    -- | Instantiate a code ROM of @capacity@ bytes with initial 16-bit word contents.
    -- The capacity is the hardware ROM size in bytes; initial data may be smaller —
    -- unused words are zero-padded.  Returns a 'BusDef' token for address-space placement.
    mkRomP :: ( KnownNat capacity, KnownNat n
              , KnownNat (Div capacity 2)
              , n <= Div capacity 2 )
           => Proxy capacity -> Vec n (BitVector 16) -> m (BusDef ())

    -- | Connect a Harvard CPU as bus master, returning the master's bus signals.
    --
    -- @codeBus@ carries the ROM token (from 'mkRom'); @dataBus@ carries the
    -- peripheral and RAM layout.  @irqs@ is the interrupt controller output.
    --
    -- The returned 'BusMaster' carries the CPU's drive signals in the synthesis
    -- path and is a placeholder in the spec path.
    harvestCPU
        :: (HarvardCPU cpu, BusArch arch)
        => cpu
        -> Bus arch         -- ^ code-bus layout and architecture
        -> Bus arch         -- ^ data-bus layout and architecture
        -> IrqDef sig
        -> m (BusMaster sig dat)

    -- | Inject explicit signals as the bus master.
    --
    -- Alternative to 'harvestCPU' for testing or non-CPU bus drivers.
    -- The @dataBus@ argument records the address layout (needed so 'mkPeriph'
    -- and 'mkRam' can resolve their base addresses).
    -- In the spec path this is a no-op.
    injectBusMaster
        :: BusArch arch
        => sig (Maybe Word32)          -- ^ read address driven onto bus
        -> sig (Maybe (Word32, dat))   -- ^ write command driven onto bus
        -> sig (Unsigned 16)           -- ^ code address (zero if no ROM)
        -> Bus arch                    -- ^ data-bus layout and architecture
        -> m ()

-- | Sugar over 'harvestCPU'.
simpleHarvardCore
    :: (SystemDSL m sig dat, HarvardCPU cpu, BusArch arch)
    => cpu
    -> Bus arch
    -> Bus arch
    -> IrqDef sig
    -> m (BusMaster sig dat)
simpleHarvardCore = harvestCPU

-- | Instantiate a RAM of @n@ bytes. Use as @mkRam \@1024@.
mkRam :: forall n m sig dat. (KnownNat n, SystemDSL m sig dat) => m (BusDef ())
mkRam = mkRamP (Proxy @n)

-- | Instantiate a code ROM of @capacity@ bytes with initial 16-bit word contents.
-- Use as @mkRom \@4096 myProgram@.
mkRom :: forall capacity n m sig dat.
         ( KnownNat capacity, KnownNat n
         , KnownNat (Div capacity 2)
         , n <= Div capacity 2
         , SystemDSL m sig dat )
      => Vec n (BitVector 16) -> m (BusDef ())
mkRom = mkRomP (Proxy @capacity)

-- | Declare an external input port. Use as @externalIn \@\"clk\"@.
externalIn :: forall name t m sig dat. (KnownSymbol name, SystemDSL m sig dat) => m (sig t)
externalIn = externalInP (Proxy @name)

-- | Declare an external output port. Use as @externalOut \@\"led\" sig@.
externalOut :: forall name t m sig dat. (KnownSymbol name, SystemDSL m sig dat) => sig t -> m ()
externalOut = externalOutP (Proxy @name)

-- ---------------------------------------------------------------------------
-- SpecWriter interpreter
-- ---------------------------------------------------------------------------

-- | Pure documentation / spec interpreter for 'SystemDSL'.
-- Records ports, peripheral metadata, and memory layout.
-- Never in the Clash synthesis path.
newtype SpecWriter a = SpecWriter { unSpec :: State SystemSpec a }
    deriving newtype (Functor, Applicative, Monad)

-- | Run a 'SpecWriter' and return the accumulated 'SystemSpec'.
runSpecWriter :: SpecWriter a -> (a, SystemSpec)
runSpecWriter sw =
    let (a, spec) = runState (unSpec sw) emptySpec
    in (a, spec { ssComponents = reverse (ssComponents spec) })

pushComp :: ComponentSpec -> SpecWriter ()
pushComp c = SpecWriter $ modify $ \s -> s { ssComponents = c : ssComponents s }

instance SystemDSL SpecWriter NullSig (BitVector 8) where

    externalInP p = do
        pushComp (SpecPort (symbolVal p) PIn PW1)
        pure NullSig

    externalOutP p _ =
        pushComp (SpecPort (symbolVal p) POut PW1)

    mkPeriph def = do
        n <- SpecWriter $ gets ssPeriphCount
        SpecWriter $ modify $ \s -> s { ssPeriphCount = n + 1 }
        let (physOut, _rdData, spec) = runPeriphDef nullOps NullSig NullSig def
            bp = BusPeriph { bpId = n, bpSpec = spec, bpSize = specSize spec }
        pure (periph bp, physOut)

    mkRamP proxy = do
        sid <- SpecWriter $ gets ssPeriphCount
        SpecWriter $ modify $ \s -> s { ssPeriphCount = sid + 1 }
        let sz = fromIntegral (natVal proxy)
        pure (ramSegment sz sid)

    mkRomP proxy _initWords = do
        let sz = fromIntegral (natVal proxy)
        pure (romBlock sz)

    harvestCPU _cpu codeBus dataBus _irqs = do
        pushComp (SpecCPU "HarvardCPU")
        let ((), codeSpecs) = runBusDef (getBusDef codeBus)
            ((), dataSpecs) = runBusDef (getBusDef dataBus)
        mapM_ pushComp codeSpecs
        mapM_ pushComp dataSpecs
        pure (BusMaster NullSig NullSig NullSig)

    injectBusMaster _ _ _ dataBus = do
        let ((), dataSpecs) = runBusDef (getBusDef dataBus)
        mapM_ pushComp dataSpecs
