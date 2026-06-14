-- NB: NoImplicitPrelude is active from cabal common-options.
module Isacle.System.Circuit
    ( synthOps
    , runSynthPeriph
    , runPeriphSynth
    ) where

import Prelude
import Clash.Prelude (Signal, KnownDomain, NFDataX, Clock, Reset, Enable,
                      HiddenClockResetEnable, hasClock, hasReset, hasEnable)
import Data.Word (Word8)

import qualified Clash.Explicit.Prelude as CE

import Isacle.System.Periph

-- ---------------------------------------------------------------------------
-- Synthesis ops (explicit clock/reset/enable — no HiddenClockResetEnable)
-- ---------------------------------------------------------------------------

-- | 'PeriphOps' for Clash synthesis using explicit clock, reset, and enable.
--
-- Callers supply the 'Clock', 'Reset', and 'Enable' extracted at the
-- 'runSynth' call site (where 'HiddenClockResetEnable' is in scope).
-- This avoids storing a rank-2 function in the synthesis state.
synthOps :: (KnownDomain dom, NFDataX dat)
         => Clock dom -> Reset dom -> Enable dom
         -> PeriphOps (Signal dom) dat
synthOps clk rst en = PeriphOps { sigReg = CE.regEn clk rst en }

-- ---------------------------------------------------------------------------
-- Synthesis runners
-- ---------------------------------------------------------------------------

-- | Run a peripheral definition in Clash synthesis mode, producing actual
-- hardware.  The 'PeriphOps' record carries the register-creation function.
runPeriphSynth
    :: forall p dom addr dat a.
       ( Integral addr, Num addr, Eq addr
       , NFDataX dat, Num dat
       )
    => PeriphOps (Signal dom) dat         -- ^ ops (e.g. 'synthOps' at call site)
    -> addr                               -- ^ peripheral base address
    -> Signal dom (Maybe (addr, dat))     -- ^ write bus (full addresses)
    -> Signal dom (Maybe addr)            -- ^ read bus
    -> PeriphDef p (Signal dom) dat a     -- ^ peripheral definition
    -> (a, Signal dom dat)                -- ^ (physical outputs, read data signal)
runPeriphSynth ops base wrBus rdBus periph =
    let offsetWr :: Signal dom (Maybe (Word8, dat))
        offsetWr = fmap (fmap (\(a, d) -> (fromIntegral (a - base), d))) wrBus

        offsetRd :: Signal dom (Maybe Word8)
        offsetRd = fmap (fmap (\a -> fromIntegral (a - base))) rdBus

        (physOut, rdData, _spec) = runPeriphDef ops offsetWr offsetRd periph

    in (physOut, rdData)

-- | Convenience wrapper: uses 'synthOps' with the hidden 'HiddenClockResetEnable'.
--
-- Address filtering is the bus decoder's responsibility; stray writes to
-- out-of-range offsets are harmless (they won't match any register).
runSynthPeriph
    :: forall p dom addr dat a.
       ( KnownDomain dom
       , HiddenClockResetEnable dom
       , NFDataX dat, Num dat
       , Integral addr, Num addr, Eq addr
       )
    => addr                               -- ^ peripheral base address
    -> Signal dom (Maybe (addr, dat))     -- ^ write bus (full addresses)
    -> Signal dom (Maybe addr)            -- ^ read bus
    -> PeriphDef p (Signal dom) dat a     -- ^ peripheral definition
    -> (a, Signal dom dat)                -- ^ (physical outputs, read data signal)
runSynthPeriph = runPeriphSynth (synthOps hasClock hasReset hasEnable)
