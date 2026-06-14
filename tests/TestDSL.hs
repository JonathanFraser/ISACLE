{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Prelude
import Clash.Sized.BitVector (BitVector)

import Isacle.System
import Isacle.CPU.Dummy (DummyCore(..))
import Isacle.Periph.GPIO (gpioDef)
import Isacle.Periph.UART (uartSpecPeriphDef)

-- ---------------------------------------------------------------------------
-- System description
--
-- mkPeriph  — instantiate peripherals, binding physical signals
-- attach     — lay out the address space
-- mkRam      — instantiate a block RAM (monadic; synthesis-capable)
-- simpleHarvardCore — connect a CPU bus master
--
-- Returns GPIO port/ddr outputs so the top entity can expose them.
-- ---------------------------------------------------------------------------

mySystem
    :: SystemDSL m sig (BitVector 8)
    => sig (BitVector 8)   -- ^ GPIO physical pin inputs
    -> m ( sig (BitVector 8)   -- ^ GPIO PORT latch
         , sig (BitVector 8)   -- ^ GPIO DDR
         )
mySystem gpioIn = do
    (gpioBus, (gpioPort, gpioDdr)) <- mkPeriph (gpioDef gpioIn)

    rxPin <- externalIn @"uart_rx"
    (uartBus, (txLine, _rxIrq, _txIrq)) <- mkPeriph (uartSpecPeriphDef rxPin)
    externalOut @"uart_tx" txLine

    dataRam <- mkRam @2048

    let codeBus = mkBus SimpleBus $
            annotate @".text" $
                attach 0x0000 (romBlock 0x2000)

    let dataBus = mkBus SimpleBus $ do
            label @"periph" $ attach 0x0100 $ do
                label @"gpio" $ attach 0x00 gpioBus
                label @"uart" $ attach 0x10 uartBus
            label @"ram" $
                attach 0x8000 dataRam

    _ <- simpleHarvardCore DummyCore codeBus dataBus noIrqs
    return (gpioPort, gpioDdr)

-- ---------------------------------------------------------------------------
-- Run the spec interpreter and print artifacts
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
    let (_, spec) = runSpecWriter (mySystem NullSig)
    putStrLn (extractMemoryMap spec)
    putStrLn (genCHeader "memmap" spec)
