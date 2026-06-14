-- NB: NoImplicitPrelude is active from cabal common-options.
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Isacle.System.Synth
    ( -- * Synthesis interpreter
      Synth
    , runSynth
    , runSynthLazy
      -- * Spec-pass utilities (independent of synthesis)
    , specFeed
    , specAddrMap
    , specAddrList
      -- * Bus DSL for direct synthesis (no address map)
    , BusDSL
    , SynthBusArch(..)
    , Master(..)
    , busAttach
    ) where

import Prelude
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Word (Word8, Word32)

import Clash.Prelude hiding (replicate, (&&), (++), concatMap)
import qualified Clash.Sized.Vector as CV
import qualified Clash.Explicit.Prelude as CE

import Isacle.System.Spec
import Isacle.System.Periph
import Isacle.System.BusDef (Bus(..), BusPeriph(..), BusMaster(..), periph, ramSegment, romBlock, runBusDef)
import Isacle.System.BusArch (BusArch, SimpleBus(..))
import Isacle.System.Builder

-- ---------------------------------------------------------------------------
-- Per-component address feed: (base, size) keyed by the component ID.
--
-- Set by harvestCPU / injectBusMaster from the dataBus specs.
-- Read lazily by mkPeriph / mkRamP via envPeriphFeed.
-- ---------------------------------------------------------------------------

type PeriphFeed = IntMap (Word32, Word32)

-- | Build a feed table from the component specs in a dataBus.
buildFeed :: [ComponentSpec] -> PeriphFeed
buildFeed = IM.fromList . concatMap entry
  where
    entry (SpecPeriph base sz _ pid) = [(pid, (base, sz))]
    entry (SpecRAM    base sz _ sid) = [(sid, (base, sz))]
    entry _                          = []

-- ---------------------------------------------------------------------------
-- Pure synthesis state
-- ---------------------------------------------------------------------------

data SynthPureSt = SynthPureSt
    { spPeriphCount :: Int
    }

-- ---------------------------------------------------------------------------
-- Signal environment — all Signal fields, fully representable.
--
-- envPeriphFeed is set lazily in runSynth to stPeriphFeed finalSt.
-- mkPeriph / mkRamP read it inside lambdas so it is never forced during the
-- monad run; only Clash's signal-graph traversal (or simulation sampling)
-- forces it, at which point finalSt is already fully computed.
-- ---------------------------------------------------------------------------

data SynthEnv dom = SynthEnv
    { envWrBus      :: Signal dom (Maybe (Word32, BitVector 8))
    , envRdAddr     :: Signal dom (Maybe Word32)
    , envCodeAddr   :: Signal dom (Unsigned 16)
    , envPeriphFeed :: PeriphFeed   -- lazy: stPeriphFeed finalSt
    }

-- ---------------------------------------------------------------------------
-- Synthesis state
-- ---------------------------------------------------------------------------

data SynthSt dom = SynthSt
    { stPure       :: SynthPureSt
    , stClock      :: Clock dom
    , stReset      :: Reset dom
    , stEnable     :: Enable dom
    , stRdData     :: Signal dom (BitVector 8)    -- accumulated periph read data
    , stCodeData   :: Signal dom (BitVector 16)
    , stMaster     :: BusMaster (Signal dom) (BitVector 8)
    , stPeriphFeed :: PeriphFeed   -- written by harvestCPU / injectBusMaster
    }

-- ---------------------------------------------------------------------------
-- Synth monad
-- ---------------------------------------------------------------------------

newtype Synth dom a = Synth
    { unSynth :: SynthEnv dom -> SynthSt dom -> (a, SynthSt dom) }

instance Functor (Synth dom) where
    {-# INLINE fmap #-}
    fmap f (Synth g) = Synth $ \env st ->
        let (a, st') = g env st in (f a, st')

instance Applicative (Synth dom) where
    {-# INLINE pure #-}
    pure a = Synth $ \_ st -> (a, st)
    {-# INLINE (<*>) #-}
    Synth gf <*> Synth gx = Synth $ \env st ->
        let (f, st')  = gf env st
            (x, st'') = gx env st'
        in (f x, st'')

instance Monad (Synth dom) where
    {-# INLINE return #-}
    return = pure
    {-# INLINE (>>=) #-}
    Synth m >>= k = Synth $ \env st ->
        let (a, st') = m env st
        in unSynth (k a) env st'

-- ---------------------------------------------------------------------------
-- SystemDSL instance
-- ---------------------------------------------------------------------------

instance KnownDomain dom => SystemDSL (Synth dom) (Signal dom) (BitVector 8) where

    externalInP _proxy = Synth $ \_ st ->
        (pure (errorX "externalIn: wire this port at the top-level entity"), st)
    {-# INLINE externalInP #-}

    externalOutP _proxy _sig = Synth $ \_ st -> ((), st)
    {-# INLINE externalOutP #-}

    -- Peripheral synthesis:
    --
    -- 1. Run the peripheral definition once with dummy signals to collect the
    --    PeriphSpec (register layout) and derive the correct address-window
    --    size.  This pure pass never touches the lazy knot.
    --
    -- 2. Compute 'base' as a lazy thunk: it captures envPeriphFeed (which
    --    points at stPeriphFeed finalSt) but is only forced inside the Signal
    --    lambdas of filteredWr / filteredRd.  Those lambdas execute during
    --    simulation or Clash synthesis — after the monad has fully run and
    --    finalSt is available.
    --
    -- 3. Address decode is centralised here: filteredWr / filteredRd carry
    --    Nothing for any address outside [base, base+sz).  Peripherals see
    --    only transactions addressed to them.
    mkPeriph (pdef :: PeriphDef p (Signal dom) (BitVector 8) (PhysOutputs p (Signal dom))) = Synth $ \env st ->
        let ps  = stPure st
            n   = spPeriphCount ps

            -- Address-window size from the type-level constant — Clash can
            -- evaluate this trivially without running PeriphDef at all.
            sz :: Word32
            sz  = fromIntegral (natVal (Proxy @(PeriphSize p)))

            bp  = BusPeriph { bpId = n, bpSpec = PeriphSpec [], bpSize = sz }

            -- base is lazy: forced only inside Signal lambdas below.
            base :: Word32
            base = fst $ fromMaybe (0, sz) (IM.lookup n (envPeriphFeed env))

            -- Range-filtered, offset-relative strobes for this peripheral.
            -- Nothing for any address outside [base, base+sz).
            filteredWr :: Signal dom (Maybe (Word8, BitVector 8))
            filteredWr = fmap (\case
                            Just (a, d) | a >= base && a < base + sz
                                -> Just (fromIntegral (a - base), d)
                            _ -> Nothing)
                            (envWrBus env)

            filteredRd :: Signal dom (Maybe Word8)
            filteredRd = fmap (\case
                            Just a | a >= base && a < base + sz
                                -> Just (fromIntegral (a - base))
                            _ -> Nothing)
                            (envRdAddr env)

            ops = PeriphOps { sigReg = CE.regEn (stClock st) (stReset st) (stEnable st) }
            (physOut, rdData, _) = runPeriphDef ops filteredWr filteredRd pdef
            st' = st { stPure   = ps { spPeriphCount = n + 1 }
                     , stRdData = liftA2 (+) (stRdData st) rdData }
        in ((periph bp, physOut), st')
    {-# INLINE mkPeriph #-}

    -- RAM base address and size come from envPeriphFeed (same lazy knot).
    -- Both are used inside Signal lambdas; neither is forced during the monad.
    mkRamP proxy = Synth $ \env st ->
        let ps   = stPure st
            sid  = spPeriphCount ps
            sz   = fromIntegral (natVal proxy)
            (base, ramSz) = fromMaybe (0, sz) (IM.lookup sid (envPeriphFeed env))

            toIdx :: Word32 -> Int
            toIdx addr = fromIntegral (bitCoerce (addr - base) :: Unsigned 32)

            rdIdx = fmap (\a -> case a of
                        Just addr | addr >= base && addr < base + ramSz -> toIdx addr
                        _                                                -> 0)
                        (envRdAddr env)

            wrCmd = fmap (\case
                        Just (addr, d) | addr >= base && addr < base + ramSz
                            -> Just (toIdx addr, d)
                        _ -> Nothing)
                        (envWrBus env)

            rdData = CE.blockRam (stClock st) (stEnable st)
                        (CV.replicate (snatProxy proxy) 0) rdIdx wrCmd

            st'  = st { stPure   = ps { spPeriphCount = sid + 1 }
                      , stRdData = liftA2 (+) (stRdData st) rdData }
        in (ramSegment sz sid, st')
    {-# INLINE mkRamP #-}

    mkRomP proxy initWords = Synth $ \env st ->
        let sz       = fromIntegral (natVal proxy)
            codeData = fmap (asyncRom initWords) (envCodeAddr env)
        in (romBlock sz, st { stCodeData = codeData })
    {-# INLINE mkRomP #-}

    -- harvestCPU wires the CPU and builds the per-component feed from dataBus.
    -- The feed (pure Word32 pairs) is set in stPeriphFeed so that the lazy
    -- envPeriphFeed knot resolves after the monad completes.
    harvestCPU cpu _codeBus dataBus irqs = Synth $ \_ st ->
        let ((), specs) = runBusDef (getBusDef dataBus)
            feed   = buildFeed specs
            master = cpuSynthWire cpu
                        (stClock  st) (stReset st) (stEnable st)
                        (runIrqDef irqs)
                        (stCodeData st)
                        (stRdData   st)
            st' = st { stMaster    = master
                     , stPeriphFeed = feed }
        in (master, st')
    {-# INLINE harvestCPU #-}

    -- In the runSynth path the feed is already in envPeriphFeed (supplied by
    -- the caller before the monad runs), so there is no need to re-derive it
    -- from dataBus here.  Ignoring dataBus keeps the [ComponentSpec] traversal
    -- (buildFeed / IM.fromList) out of the synthesis-visible Isacle, which would
    -- otherwise cause Clash's specialisation limit to be exceeded.
    injectBusMaster rdA wrB codeA _ = Synth $ \_ st ->
        let master = BusMaster { bmRdAddr = rdA, bmWrBus = wrB, bmCodeAddr = codeA }
            st' = st { stMaster = master }
        in ((), st')
    {-# INLINE injectBusMaster #-}

-- ---------------------------------------------------------------------------
-- Spec-pass utilities (independent of synthesis)
-- ---------------------------------------------------------------------------

-- | Extract the full (base, size) address feed from a spec pass.
-- Use this to pre-compute addresses before calling 'runSynth'.
specFeed :: SpecWriter a -> PeriphFeed
specFeed sw =
    let (_, sspec) = runSpecWriter sw
    in buildFeed (ssComponents sspec)

specAddrMap :: SpecWriter a -> IntMap Word32
specAddrMap sw =
    let (_, sspec) = runSpecWriter sw
    in IM.fromList [ (pid, base) | SpecPeriph base _ _ pid <- ssComponents sspec ]
      `IM.union`
       IM.fromList [ (sid, base) | SpecRAM    base _ _ sid <- ssComponents sspec ]

specAddrList :: SpecWriter a -> [(Int, Word32)]
specAddrList sw =
    let (_, sspec) = runSpecWriter sw
    in [ (pid, base) | SpecPeriph base _ _ pid <- ssComponents sspec ]
    ++ [ (sid, base) | SpecRAM    base _ _ sid <- ssComponents sspec ]

-- ---------------------------------------------------------------------------
-- Synthesis runner — explicit feed and bus master (no lazy knot)
--
-- Clash-safe: no letrec over SynthSt.  Use this for systems driven by an
-- external bus master (injectBusMaster).  Compute the feed first:
--
--   feed   = specFeed (yourSystem NullSig ... NullSig)
--   master = BusMaster wrCmd rdAddr (pure 0)
--   result = runSynth feed master (yourSystem sig1 ... sigN)
-- ---------------------------------------------------------------------------

{-# INLINE runSynth #-}
runSynth
    :: forall dom a.
       ( HiddenClockResetEnable dom
       , KnownDomain dom
       , NFDataX (BitVector 8)
       )
    => PeriphFeed                               -- ^ pre-computed from 'specFeed'
    -> BusMaster (Signal dom) (BitVector 8)    -- ^ external bus master signals
    -> Synth dom a
    -> a
runSynth feed master buildSystem = result
  where
    env :: SynthEnv dom
    env = SynthEnv
        { envWrBus      = bmWrBus    master
        , envRdAddr     = bmRdAddr   master
        , envCodeAddr   = bmCodeAddr master
        , envPeriphFeed = feed
        }

    initialSt :: SynthSt dom
    initialSt = SynthSt
        { stPure       = SynthPureSt { spPeriphCount = 0 }
        , stClock      = hasClock
        , stReset      = hasReset
        , stEnable     = hasEnable
        , stRdData     = pure 0
        , stCodeData   = pure (errorX "runSynth: call mkRom before harvestCPU")
        , stMaster     = master
        , stPeriphFeed = feed
        }

    (result, _) = unSynth buildSystem env initialSt

-- ---------------------------------------------------------------------------
-- Synthesis runner — lazy knot version (for harvestCPU / CPU-based systems)
--
-- The lazy knot:
--   env.envWrBus      = bmWrBus    (stMaster    finalSt)   -- Signal lazy
--   env.envRdAddr     = bmRdAddr   (stMaster    finalSt)   -- Signal lazy
--   env.envCodeAddr   = bmCodeAddr (stMaster    finalSt)   -- Signal lazy
--   env.envPeriphFeed = stPeriphFeed finalSt               -- PeriphFeed lazy
--
-- Works at GHC runtime (lazy evaluation resolves the knot), but Clash cannot
-- synthesize this form because SynthSt is not hardware-representable.
-- Use only for simulation / testing of CPU-based designs, not for synthesis.
-- ---------------------------------------------------------------------------

{-# NOINLINE runSynthLazy #-}
runSynthLazy
    :: forall dom a.
       ( HiddenClockResetEnable dom
       , KnownDomain dom
       , NFDataX (BitVector 8)
       )
    => Synth dom a
    -> a
runSynthLazy buildSystem = result
  where
    noMaster :: BusMaster (Signal dom) (BitVector 8)
    noMaster = BusMaster (pure Nothing) (pure Nothing) (pure 0)

    initialSt :: SynthSt dom
    initialSt = SynthSt
        { stPure       = SynthPureSt { spPeriphCount = 0 }
        , stClock      = hasClock
        , stReset      = hasReset
        , stEnable     = hasEnable
        , stRdData     = pure 0
        , stCodeData   = pure (errorX "runSynthLazy: call mkRom before harvestCPU")
        , stMaster     = noMaster
        , stPeriphFeed = IM.empty
        }

    env :: SynthEnv dom
    env = SynthEnv
        { envWrBus      = bmWrBus    (stMaster    finalSt)
        , envRdAddr     = bmRdAddr   (stMaster    finalSt)
        , envCodeAddr   = bmCodeAddr (stMaster    finalSt)
        , envPeriphFeed = stPeriphFeed finalSt
        }

    (result, finalSt) = unSynth buildSystem env initialSt

-- ---------------------------------------------------------------------------
-- Bus DSL — Clash-safe direct synthesis, no address map
--
-- A bus is a pair of master signals (write command, read address) plus an
-- accumulated response (OR of all peripheral read-data).  All fields are
-- Signal dom — fully representable, no InlineNonRep loop.
-- ---------------------------------------------------------------------------

data SynthBus dom dat = SynthBus
    { sbWr   :: Signal dom (Maybe (Word32, dat))
    , sbRd   :: Signal dom (Maybe Word32)
    , sbResp :: Signal dom dat
    }

newtype BusDSL dom dat a = BusDSL
    { unBusDSL :: SynthBus dom dat -> (a, Signal dom dat) }

instance Functor (BusDSL dom dat) where
    {-# INLINE fmap #-}
    fmap f (BusDSL g) = BusDSL $ \bus ->
        let (a, resp) = g bus in (f a, resp)

instance (Num dat, Bits dat) => Applicative (BusDSL dom dat) where
    {-# INLINE pure #-}
    pure a = BusDSL $ \_ -> (a, pure 0)
    {-# INLINE (<*>) #-}
    BusDSL gf <*> BusDSL gx = BusDSL $ \bus ->
        let (f, respF) = gf bus
            (x, respX) = gx bus
        in (f x, liftA2 (.|.) respF respX)

instance (Num dat, Bits dat) => Monad (BusDSL dom dat) where
    {-# INLINE return #-}
    return = pure
    {-# INLINE (>>=) #-}
    BusDSL m >>= k = BusDSL $ \bus ->
        let (a, respA) = m bus
            (b, respB) = unBusDSL (k a) bus
        in (b, liftA2 (.|.) respA respB)

{-# INLINE runSynthBus #-}
runSynthBus
    :: (Num dat, Bits dat)
    => Signal dom (Maybe (Word32, dat))
    -> Signal dom (Maybe Word32)
    -> BusDSL dom dat a
    -> a
runSynthBus wr rd action = fst $ unBusDSL action (SynthBus wr rd (pure 0))

-- ---------------------------------------------------------------------------
-- SynthBusArch — typeclass linking bus architectures to their master bundles
--
-- Each bus architecture declares its 'Master' signal record and how to
-- decode it into the canonical (wr, rd) pair that BusDSL expects.
-- Adding a new architecture (e.g. Wishbone) requires only a new instance.
-- ---------------------------------------------------------------------------

class BusArch arch => SynthBusArch arch where
    data Master arch (dom :: Domain) dat

    -- | Decode the master bundle into canonical (wr, rd) signals.
    masterWires :: Master arch dom dat
                -> ( Signal dom (Maybe (Word32, dat))
                   , Signal dom (Maybe Word32) )

    -- | Run a BusDSL action, discarding the accumulated read-data.
    -- Use when no CPU needs to consume bus responses.
    {-# INLINE runBus #-}
    runBus :: (Num dat, Bits dat) => Master arch dom dat -> BusDSL dom dat a -> a
    runBus m action = fst $ runBusR m action

    -- | Run a BusDSL action, returning both the result and the accumulated
    -- read-data signal.  Wire the read-data back into a CPU's data input.
    {-# INLINE runBusR #-}
    runBusR :: (Num dat, Bits dat)
            => Master arch dom dat -> BusDSL dom dat a -> (a, Signal dom dat)
    runBusR m action = unBusDSL action (SynthBus wr rd (pure 0))
      where (wr, rd) = masterWires m

instance SynthBusArch SimpleBus where
    data Master SimpleBus dom dat = SimpleBusMaster
        { smAddr :: Signal dom (BitVector 32)
        , smWen  :: Signal dom Bool
        , smWdat :: Signal dom dat
        , smRen  :: Signal dom Bool
        }
    masterWires m =
        ( (\a e d -> if e then Just (bitCoerce a, d) else Nothing)
          <$> smAddr m <*> smWen m <*> smWdat m
        , (\a e -> if e then Just (bitCoerce a) else Nothing)
          <$> smAddr m <*> smRen m
        )

-- | Attach a peripheral at a literal base address, returning its physical outputs.
{-# INLINE busAttach #-}
busAttach
    :: forall p dom dat.
       ( HiddenClockResetEnable dom
       , KnownDomain dom
       , HasPhysIO p
       , KnownNat (PeriphSize p)
       , Num dat
       , Bits dat
       , BitPack dat
       , NFDataX dat
       )
    => Word32
    -> PeriphDef p (Signal dom) dat (PhysOutputs p (Signal dom))
    -> BusDSL dom dat (PhysOutputs p (Signal dom))
busAttach lo pdef = BusDSL $ \bus ->
    let sz :: Word32
        sz  = fromIntegral (natVal (Proxy @(PeriphSize p)))
        ops = PeriphOps { sigReg = CE.regEn hasClock hasReset hasEnable }
        filteredWr = fmap (\case
                        Just (a, d) | a >= lo && a < lo + sz
                            -> Just (fromIntegral (a - lo), d)
                        _ -> Nothing)
                        (sbWr bus)
        filteredRd = fmap (\case
                        Just a | a >= lo && a < lo + sz
                            -> Just (fromIntegral (a - lo))
                        _ -> Nothing)
                        (sbRd bus)
        (physOut, rdData, _) = runPeriphDef ops filteredWr filteredRd pdef
    in (physOut, rdData)
