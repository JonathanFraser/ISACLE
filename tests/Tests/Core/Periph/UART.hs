module Tests.Core.Periph.UART where

import Prelude
import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.Hedgehog
import qualified Hedgehog as H
import qualified Clash.Prelude as C

import Core.Periph.UART

type UAddr = C.Unsigned 16
type UData = C.Unsigned 8

uartBase :: UAddr
uartBase = 0x80  -- UDR=0x80, USR=0x81, UBRR=0x82

-- | Run the UART for n cycles with explicit per-cycle inputs.
runUART
    :: Int
    -> [Bool]                         -- Rx line
    -> [Maybe UAddr]                  -- bus reads
    -> [Maybe (UAddr, UData)]         -- bus writes
    -> ([UData], [Bool], [Bool], [Bool])  -- (rdData, txLine, rxIrq, txIrq)
runUART n rxs rds wrs =
    let pad xs = xs ++ repeat (last xs)
        (rd, tx, rxIrq, txIrq) =
            C.withClockResetEnable (C.clockGen @C.System) C.resetGen C.enableGen
                (uartUnit uartBase 434
                    (C.fromList (pad rxs))
                    (C.fromList (pad rds))
                    (C.fromList (pad wrs)))
    in (C.sampleN n rd, C.sampleN n tx, C.sampleN n rxIrq, C.sampleN n txIrq)

-- On reset the Tx line must be idle-high.
prop_uart_tx_idle_high :: H.Property
prop_uart_tx_idle_high = H.withTests 1 . H.property $ do
    let (_, tx, _, _) = runUART 4 (repeat True) (repeat Nothing) (repeat Nothing)
    H.assert (all (== True) tx)

-- UDRE (txIrq / bit 0 of USR) is set when no byte is pending.
prop_uart_udre_set_when_idle :: H.Property
prop_uart_udre_set_when_idle = H.withTests 1 . H.property $ do
    let (_, _, _, txIrq) = runUART 3 (repeat True) (repeat Nothing) (repeat Nothing)
    H.assert (all (== True) txIrq)

-- Writing UDR clears UDRE (Tx buffer now occupied).
prop_uart_udre_clears_after_write :: H.Property
prop_uart_udre_clears_after_write = H.withTests 1 . H.property $ do
    let (_, _, _, txIrq) = runUART 3
            (repeat True)
            (repeat Nothing)
            [Nothing, Just (0x80, 0x41), Nothing]
    -- UDRE should be False in the cycle after the write.
    H.assert (False `elem` txIrq)

-- Tx line goes low on the start bit immediately after the baud period begins.
prop_uart_tx_start_bit :: H.Property
prop_uart_tx_start_bit = H.withTests 1 . H.property $ do
    -- Use a tiny baud divisor (2) so start bit arrives quickly.
    let (_, tx, _, _) = runUART 10
            (repeat True)
            (repeat Nothing)
            [ Nothing
            , Just (0x82, 2)      -- UBRR = 2
            , Just (0x80, 0x00)   -- write 0x00 to UDR
            , Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing
            ]
    -- At some point the line should go low (start bit).
    H.assert (False `elem` tx)

-- Reading UBRR reflects the written value.
prop_uart_read_ubrr :: H.Property
prop_uart_read_ubrr = H.withTests 1 . H.property $ do
    let (rd, _, _, _) = runUART 4
            (repeat True)
            [Nothing, Nothing, Just 0x82, Just 0x82]
            [Nothing, Just (0x82, 50), Nothing, Nothing]
    H.assert (50 `elem` rd)

uartTests :: TestTree
uartTests = $(testGroupGenerator)
