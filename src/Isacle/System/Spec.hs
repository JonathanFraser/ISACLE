-- NB: NoImplicitPrelude is active from cabal common-options.
module Isacle.System.Spec
    ( SystemSpec(..)
    , emptySpec
    , ComponentSpec(..)
    , PortDir(..)
    , PortWidth(..)
    , ROMHandle(..)
    , RAMHandle(..)
    , InterruptHandle(..)
    , NullSig(..)
    ) where

import Prelude
import Data.Kind (Type)
import Data.Word (Word32)
import GHC.TypeLits (Nat)

-- | Direction of a top-level port.
data PortDir = PIn | POut | PInOut deriving (Show, Eq)

-- | Width of a top-level port.
data PortWidth = PW1 | PW8 | PW16 | PW32 deriving (Show, Eq)

-- | Description of a single component in the system.
data ComponentSpec
    = SpecROM    Word32 Word32 String      -- ^ base address, size (bytes), name
    | SpecRAM    Word32 Word32 String Int  -- ^ base address, size (bytes), name, seg-id
    | SpecPeriph Word32 Word32 String Int  -- ^ base address, size (bytes), name, periph-id
    | SpecCPU    String                    -- ^ CPU / bus master name
    | SpecPort   String PortDir PortWidth
    deriving (Show)

-- | Accumulated system description built by the 'SystemDSL' builder monad.
data SystemSpec = SystemSpec
    { ssComponents  :: [ComponentSpec]
    , ssPeriphCount :: Int               -- ^ next segment ID to assign (peripherals + RAMs)
    } deriving (Show)

emptySpec :: SystemSpec
emptySpec = SystemSpec [] 0

-- ---------------------------------------------------------------------------
-- Typed handles
-- ---------------------------------------------------------------------------

newtype ROMHandle (n :: Nat) word = ROMHandle { romBase :: Word32 } deriving (Show)
newtype RAMHandle (n :: Nat) word = RAMHandle { ramBase :: Word32 } deriving (Show)
newtype InterruptHandle (n :: Nat) = InterruptHandle { intBase :: Word32 } deriving (Show)

-- ---------------------------------------------------------------------------
-- Null signal placeholder (analysis / spec interpreter)
-- ---------------------------------------------------------------------------

-- | Phantom signal for the spec / documentation interpreter.
-- Carries no runtime value; the type parameter tracks element type only.
data NullSig (a :: Type) = NullSig deriving (Show, Eq)

instance Functor NullSig where
    fmap _ _ = NullSig
    {-# INLINE fmap #-}

instance Applicative NullSig where
    pure _ = NullSig
    {-# INLINE pure #-}
    _ <*> _ = NullSig
    {-# INLINE (<*>) #-}

instance Monad NullSig where
    _ >>= _ = NullSig
    {-# INLINE (>>=) #-}
