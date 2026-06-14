-- | Minimal synthesis target: two GPIO peripherals on a SimpleBus,
-- driven by external master signals.  No CPU required.
{-# OPTIONS_GHC -Wno-orphans #-}
module Example.SimpleBusSynth where

import Clash.Prelude

import Isacle.System.BusArch (SimpleBus(..))
import Isacle.System.Synth   (SynthBusArch(..), Master(..), busAttach)
import Isacle.Periph.GPIO    (gpioDef)

createDomain vSystem{vName="DomSynth", vPeriod=hzToPeriod 50e6}

-- | Two GPIO peripherals on a simple address bus.
-- GPIO-A: PIN=0x00  DDR=0x01  PORT=0x02
-- GPIO-B: PIN=0x10  DDR=0x11  PORT=0x12
simpleBusSynth
    :: HiddenClockResetEnable dom
    => Signal dom (BitVector 8)   -- ^ gpio_a_in
    -> Signal dom (BitVector 8)   -- ^ gpio_b_in
    -> Signal dom (BitVector 32)  -- ^ bus_addr
    -> Signal dom Bool             -- ^ bus_wen
    -> Signal dom (BitVector 8)   -- ^ bus_wdat
    -> Signal dom Bool             -- ^ bus_ren
    -> ( Signal dom (BitVector 8)  -- ^ gpio_a_port
       , Signal dom (BitVector 8)  -- ^ gpio_a_ddr
       , Signal dom (BitVector 8)  -- ^ gpio_b_port
       , Signal dom (BitVector 8)  -- ^ gpio_b_ddr
       )
simpleBusSynth gpioAIn gpioBIn addr wen wdat ren =
    runBus (SimpleBusMaster addr wen wdat ren) $ do
        (portA, ddrA) <- busAttach 0x00 (gpioDef gpioAIn)
        (portB, ddrB) <- busAttach 0x10 (gpioDef gpioBIn)
        return (portA, ddrA, portB, ddrB)

{-# ANN topEntitySimpleBus
  (Synthesize
    { t_name   = "simple_bus_synth"
    , t_inputs = [ PortName "clk"
                 , PortName "rst_n"
                 , PortName "en"
                 , PortName "gpio_a_in"
                 , PortName "gpio_b_in"
                 , PortName "bus_addr"
                 , PortName "bus_wen"
                 , PortName "bus_wdat"
                 , PortName "bus_ren"
                 ]
    , t_output = PortProduct ""
                     [ PortName "gpio_a_port"
                     , PortName "gpio_a_ddr"
                     , PortName "gpio_b_port"
                     , PortName "gpio_b_ddr"
                     ]
    }) #-}

{-# OPAQUE topEntitySimpleBus #-}

topEntitySimpleBus
    :: Clock DomSynth -> Reset DomSynth -> Enable DomSynth
    -> Signal DomSynth (BitVector 8)
    -> Signal DomSynth (BitVector 8)
    -> Signal DomSynth (BitVector 32)
    -> Signal DomSynth Bool
    -> Signal DomSynth (BitVector 8)
    -> Signal DomSynth Bool
    -> ( Signal DomSynth (BitVector 8)
       , Signal DomSynth (BitVector 8)
       , Signal DomSynth (BitVector 8)
       , Signal DomSynth (BitVector 8)
       )
topEntitySimpleBus = exposeClockResetEnable simpleBusSynth
