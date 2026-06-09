module Tests.Core.Periph.Timer where

import Prelude
import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.Hedgehog
import qualified Hedgehog as H
import qualified Clash.Prelude as C

import Core.Periph.Timer

type TAddr = C.Unsigned 16
type TData = C.Unsigned 8

timerBase :: TAddr
timerBase = 0x40  -- TCCR=0x40, TCNT=0x41, OCR=0x42

runTimer
    :: Int
    -> [Bool]                        -- tick
    -> [Maybe TAddr]                 -- reads
    -> [Maybe (TAddr, TData)]        -- writes
    -> ([TData], [Bool], [Bool])     -- (rdData, ovf, cmp)
runTimer n ticks rds wrs =
    let pad xs = xs ++ repeat (last xs)
        (rd, ovf, cmp) =
            C.withClockResetEnable (C.clockGen @C.System) C.resetGen C.enableGen
                (timerUnit timerBase
                    (C.fromList (pad ticks))
                    (C.fromList (pad rds))
                    (C.fromList (pad wrs)))
    in (C.sampleN n rd, C.sampleN n ovf, C.sampleN n cmp)

-- Counter increments on each tick.
prop_timer_counts_on_tick :: H.Property
prop_timer_counts_on_tick = H.withTests 1 . H.property $ do
    let (rd, _, _) = runTimer 6
            (repeat True)
            [Nothing, Nothing, Just 0x41, Just 0x41, Just 0x41, Just 0x41]
            (repeat Nothing)
    H.assert (1 `elem` rd || 2 `elem` rd)

-- Counter does not advance when tick is False.
prop_timer_no_tick_no_advance :: H.Property
prop_timer_no_tick_no_advance = H.withTests 1 . H.property $ do
    let (rd, _, _) = runTimer 4
            (repeat False)
            [Nothing, Just 0x41, Just 0x41, Just 0x41]
            (repeat Nothing)
    H.assert (all (== 0) rd)

-- Overflow fires when counter wraps from 0xFF → 0x00 in normal mode.
prop_timer_overflow_fires_on_wrap :: H.Property
prop_timer_overflow_fires_on_wrap = H.withTests 1 . H.property $ do
    -- Pre-load TCNT to 0xFE, then tick twice: 0xFE→0xFF→0x00 (overflow).
    let (_, ovf, _) = runTimer 6
            (False : False : False : repeat True)
            (repeat Nothing)
            [ Nothing
            , Just (0x41, 0xFE)   -- write TCNT = 0xFE
            , Nothing
            , Nothing
            , Nothing
            , Nothing
            ]
    H.assert (True `elem` ovf)

-- CTC mode: counter resets to 0 and compare-match fires when TCNT == OCR.
prop_timer_ctc_resets_at_ocr :: H.Property
prop_timer_ctc_resets_at_ocr = H.withTests 1 . H.property $ do
    let (rd, _, cmp) = runTimer 8
            (False : False : False : repeat True)
            (Nothing : Nothing : Nothing : repeat (Just 0x41))
            [ Nothing
            , Just (0x40, 0x01)   -- TCCR bit 0 = CTC mode
            , Just (0x42, 0x03)   -- OCR = 3
            , Nothing, Nothing, Nothing, Nothing, Nothing
            ]
    -- Counter should reach 3 and then reset; compare-match must fire.
    H.assert (True `elem` cmp)
    -- After reset, counter should be 0 again at some point.
    H.assert (0 `elem` drop 3 rd)

-- Writing TCNT directly sets the counter.
prop_timer_write_tcnt :: H.Property
prop_timer_write_tcnt = H.withTests 1 . H.property $ do
    let (rd, _, _) = runTimer 4
            (repeat False)
            [Nothing, Nothing, Just 0x41, Just 0x41]
            [Nothing, Just (0x41, 0xAB), Nothing, Nothing]
    H.assert (0xAB `elem` rd)

-- Reading OCR reflects the last written value.
prop_timer_read_ocr :: H.Property
prop_timer_read_ocr = H.withTests 1 . H.property $ do
    let (rd, _, _) = runTimer 4
            (repeat False)
            [Nothing, Nothing, Just 0x42, Just 0x42]
            [Nothing, Just (0x42, 0x7F), Nothing, Nothing]
    H.assert (0x7F `elem` rd)

timerTests :: TestTree
timerTests = $(testGroupGenerator)
