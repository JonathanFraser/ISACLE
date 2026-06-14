-- | Minimal CPU-connected synthesis target: DummyCore drives a SimpleBus
-- with two GPIO peripherals.  Demonstrates the runBusR feedback loop.
{-# OPTIONS_GHC -Wno-orphans #-}
module Example.CpuBusSynth where

import Clash.Prelude
import qualified Clash.Explicit.Prelude as CE
import Data.Maybe (fromMaybe, isJust)

import Isacle.System.BusArch (SimpleBus(..))
import Isacle.System.BusDef  (BusMaster(..))
import Isacle.System.Builder (HarvardCPU(..), noIrqs, IrqDef(..))
import Isacle.System.Synth   (SynthBusArch(..), Master(..), busAttach)
import Isacle.CPU.Dummy      (DummyCore(..))
import Isacle.Periph.GPIO    (gpioDef)

createDomain vSystem{vName="DomCpu", vPeriod=hzToPeriod 50e6}

-- | Two GPIO peripherals, driven by DummyCore.
--
-- The feedback loop:
--   busResp  → CPU (read-data input)
--   CPU      → SimpleBusMaster (addr / wen / wdat / ren)
--   BusDSL   → busResp
--
-- DummyCore never actually reads or writes, so its bus outputs are
-- constant (Nothing).  The loop is still structurally valid: Signal dom
-- is a lazy stream, and Clash can elaborate the knot as long as there is
-- a register somewhere in the cycle (the GPIO registers close the loop).
cpuBusSynth
    :: HiddenClockResetEnable dom
    => Signal dom (BitVector 8)   -- ^ gpio_a_in
    -> Signal dom (BitVector 8)   -- ^ gpio_b_in
    -> ( Signal dom (BitVector 8)  -- ^ gpio_a_port
       , Signal dom (BitVector 8)  -- ^ gpio_a_ddr
       , Signal dom (BitVector 8)  -- ^ gpio_b_port
       , Signal dom (BitVector 8)  -- ^ gpio_b_ddr
       )
cpuBusSynth gpioAIn gpioBIn = result
  where
    -- CPU: free-running counter PC, no real data-bus transactions.
    cpu = cpuSynthWire DummyCore hasClock hasReset hasEnable
            (runIrqDef noIrqs)
            (pure 0)         -- code ROM data (unused by DummyCore)
            busResp          -- data bus read-data (feedback)

    -- Bus: address-decode the CPU's bus master outputs.
    master = SimpleBusMaster
        { smAddr = maybe 0 bitCoerce <$> bmRdAddr cpu  -- use rd addr as addr (demo)
        , smWen  = pure False
        , smWdat = pure 0
        , smRen  = isJust <$> bmRdAddr cpu
        }

    (result, busResp) = runBusR master $ do
        (portA, ddrA) <- busAttach 0x00 (gpioDef gpioAIn)
        (portB, ddrB) <- busAttach 0x10 (gpioDef gpioBIn)
        return (portA, ddrA, portB, ddrB)

{-# ANN topEntityCpuBus
  (Synthesize
    { t_name   = "cpu_bus_synth"
    , t_inputs = [ PortName "clk"
                 , PortName "rst_n"
                 , PortName "en"
                 , PortName "gpio_a_in"
                 , PortName "gpio_b_in"
                 ]
    , t_output = PortProduct ""
                     [ PortName "gpio_a_port"
                     , PortName "gpio_a_ddr"
                     , PortName "gpio_b_port"
                     , PortName "gpio_b_ddr"
                     ]
    }) #-}

{-# OPAQUE topEntityCpuBus #-}

topEntityCpuBus
    :: Clock DomCpu -> Reset DomCpu -> Enable DomCpu
    -> Signal DomCpu (BitVector 8)
    -> Signal DomCpu (BitVector 8)
    -> ( Signal DomCpu (BitVector 8)
       , Signal DomCpu (BitVector 8)
       , Signal DomCpu (BitVector 8)
       , Signal DomCpu (BitVector 8)
       )
topEntityCpuBus = exposeClockResetEnable cpuBusSynth
