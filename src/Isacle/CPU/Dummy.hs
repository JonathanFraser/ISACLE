module Isacle.CPU.Dummy
    ( DummyCore(..)
    ) where

import Prelude
import Clash.Prelude (Signal, BitVector, Unsigned, NFDataX, Clock, Reset, Enable)
import qualified Clash.Explicit.Prelude as CE

import Isacle.System.BusDef (BusMaster(..))
import Isacle.System.Builder (HarvardCPU(..))

-- | A phantom CPU type for wiring up test or demonstration SoCs.
--
--   ISA-specific packages (e.g. Clavr for AVR, clavr-8051 for MCS-51) define
--   their own real CPU kind with concrete 'BusMaster' wiring.
--   'DummyCore' is the ISACLE stand-in used when no real ISA is needed.
data DummyCore = DummyCore

instance HarvardCPU DummyCore where
    type FetchWidth DummyCore = 16

    -- | Free-running program counter; never reads or writes the data bus.
    --
    -- Uses explicit Clash primitives so no 'HiddenClockResetEnable' constraint
    -- is required inside 'cpuSynthWire'.
    cpuSynthWire _ clk rst en _irqVec _codeData _busRd =
        BusMaster
            { bmRdAddr   = pure Nothing
            , bmWrBus    = pure Nothing
            , bmCodeAddr = pc
            }
      where
        pc = CE.register clk rst en 0 (fmap (+1) pc)
