{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds       #-}
module Tests.Isacle.System.Bus where

import Prelude
import Data.Word (Word32)

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.Hedgehog
import qualified Hedgehog as H

import qualified Clash.Prelude as C

import Isacle.System
import Isacle.Periph.GPIO (gpioDef)

-- ---------------------------------------------------------------------------
-- Test system: GPIO peripheral, driven by injectBusMaster (no CPU)
-- ---------------------------------------------------------------------------

gpioBase :: Word32
gpioBase = 0x60   -- PIN=0x60, DDR=0x61, PORT=0x62

-- | A single GPIO peripheral on the data bus, driven by explicit bus signals.
-- Uses 'injectBusMaster' so no CPU type is required.
gpioBusSys
    :: SystemDSL m sig (C.BitVector 8)
    => sig (C.BitVector 8)
    -> sig (Maybe Word32)
    -> sig (Maybe (Word32, C.BitVector 8))
    -> m (sig (C.BitVector 8), sig (C.BitVector 8))  -- (PORT, DDR)
gpioBusSys gpioIn rdAddr wrBus = do
    (gpioBus, (gpioPort, gpioDdr)) <- mkPeriph (gpioDef gpioIn)
    let dataBus = mkBus SimpleBus $ label @"gpio" $ attach gpioBase gpioBus
    injectBusMaster rdAddr wrBus (pure 0) dataBus
    return (gpioPort, gpioDdr)

-- ---------------------------------------------------------------------------
-- Simulation helper
-- ---------------------------------------------------------------------------

-- | Run the synthesised bus system for @n@ cycles, returning PORT and DDR
-- samples in the same style as 'Tests.Isacle.GPIO.runGpio'.
runBusSynth
    :: Int
    -> [C.BitVector 8]
    -> [Maybe Word32]
    -> [Maybe (Word32, C.BitVector 8)]
    -> ([C.BitVector 8], [C.BitVector 8])   -- (portOut, ddrOut)
runBusSynth n pins rds wrs =
    let pad xs = xs ++ repeat (last xs)
        pinSig = C.fromList (pad pins)
        rdSig  = C.fromList (pad rds)
        wrSig  = C.fromList (pad wrs)
        feed   = specFeed (gpioBusSys NullSig NullSig NullSig)
        master = BusMaster rdSig wrSig (pure 0)
        (portSig, ddrSig) =
            C.withClockResetEnable (C.clockGen @C.System) C.resetGen C.enableGen $
                runSynth feed master (gpioBusSys pinSig rdSig wrSig)
    in (C.sampleN n portSig, C.sampleN n ddrSig)

-- ---------------------------------------------------------------------------
-- Spec-path test
-- ---------------------------------------------------------------------------

-- | 'injectBusMaster' must record the GPIO peripheral in the memory map.
prop_bus_spec_gpio_present :: H.Property
prop_bus_spec_gpio_present = H.withTests 1 . H.property $ do
    let (_, spec) = runSpecWriter $ gpioBusSys NullSig NullSig NullSig
        comps = ssComponents spec
    H.assert (not (null comps))

-- ---------------------------------------------------------------------------
-- Synthesis simulation tests
-- These mirror Tests.Isacle.GPIO but drive the bus through the DSL synthesis
-- path (Synth monad + runSynth) rather than calling gpioUnit directly.
-- ---------------------------------------------------------------------------

-- Write 0xFF to DDR; it should appear in the DDR output within a few cycles.
prop_bus_ddr_write :: H.Property
prop_bus_ddr_write = H.withTests 1 . H.property $ do
    let (_, ddrOut) = runBusSynth 4 (repeat 0)
            (repeat Nothing)
            [Just (0x61, 0xFF), Nothing, Nothing, Nothing]
    H.assert (0xFF `elem` ddrOut)

-- DDR value persists: write in cycle 1, all later samples must be 0xAA.
prop_bus_ddr_persists :: H.Property
prop_bus_ddr_persists = H.withTests 1 . H.property $ do
    let (_, ddrOut) = runBusSynth 6 (repeat 0)
            (repeat Nothing)
            (Nothing : Just (0x61, 0xAA) : repeat Nothing)
    H.assert (all (== 0xAA) (drop 2 ddrOut))

-- Write PORT; it should appear in the PORT output within a few cycles.
prop_bus_port_write :: H.Property
prop_bus_port_write = H.withTests 1 . H.property $ do
    let (portOut, _) = runBusSynth 4 (repeat 0)
            (repeat Nothing)
            [Just (0x62, 0x55), Nothing, Nothing, Nothing]
    H.assert (0x55 `elem` portOut)

-- Writing DDR must not disturb PORT, and vice versa.
prop_bus_ddr_write_no_port_side_effect :: H.Property
prop_bus_ddr_write_no_port_side_effect = H.withTests 1 . H.property $ do
    let (portOut, _) = runBusSynth 3 (repeat 0)
            (repeat Nothing)
            [Just (0x61, 0xFF), Nothing, Nothing]
    H.assert (all (== 0) portOut)

busTests :: TestTree
busTests = $(testGroupGenerator)
