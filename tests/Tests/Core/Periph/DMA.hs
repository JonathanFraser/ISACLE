module Tests.Core.Periph.DMA where

import Prelude
import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.Hedgehog
import qualified Hedgehog as H
import qualified Clash.Prelude as C

import Core.Periph.DMA
import Core.Bus (busMasterMux)

type Addr = C.Unsigned 16
type Dat  = C.Unsigned 8

-- | Run the DMA engine for n cycles.
--   @rdResp@ is the per-cycle bus read response (what memory returns).
--   @starts@ is the per-cycle start signal.
--   Returns (dmaRd, dmaWr, busy, done).
runDMA
    :: Int
    -> [Maybe (Addr, Addr, C.Unsigned 16, Bool, Bool)]  -- finite; Nothing-padded
    -> [Dat]                                             -- finite; 0-padded
    -> ( [Maybe Addr]
       , [Maybe (Addr, Dat)]
       , [Bool]
       , [Bool]
       )
runDMA n starts rdResps =
    let (rd, wr, busy, done) =
            C.withClockResetEnable (C.clockGen @C.System) C.resetGen C.enableGen
                (dmaEngine
                    (C.fromList (starts ++ repeat Nothing))
                    (C.fromList (rdResps ++ repeat 0)))
    in ( C.sampleN n rd
       , C.sampleN n wr
       , C.sampleN n busy
       , C.sampleN n done
       )

-- ---------------------------------------------------------------------------
-- Basic transfer tests
-- ---------------------------------------------------------------------------

-- DMA is idle and not busy before a transfer starts.
prop_dma_idle_not_busy :: H.Property
prop_dma_idle_not_busy = H.withTests 1 . H.property $ do
    let (_, _, busy, _) = runDMA 3
            (repeat Nothing)
            (repeat 0)
    H.assert (all (== False) busy)

-- After trigger, DMA issues a read address one cycle after the trigger.
-- Trigger at cycle 1 (post-reset); cycle 0 is the synchronous reset cycle
-- where outputs are computed but state transitions are held at initState.
prop_dma_issues_read_on_start :: H.Property
prop_dma_issues_read_on_start = H.withTests 1 . H.property $ do
    let (rd, _, _, _) = runDMA 4
            [Nothing, Just (0x100, 0x200, 1, True, True), Nothing, Nothing]
            [0,       0,    0,       0      ]
    H.assert (Just 0x100 `elem` rd)

-- DMA issues a write with the data it received from the read.
--   cycle 0: reset (idle)
--   cycle 1: trigger fires  → DMA enters DMARead, issues rdAddr=0x100
--   cycle 2: rdData=0xAB   → DMA writes (0x200,0xAB), done fires
prop_dma_writes_read_data :: H.Property
prop_dma_writes_read_data = H.withTests 1 . H.property $ do
    let (_, wr, _, _) = runDMA 5
            [Nothing, Just (0x100, 0x200, 1, True, True), Nothing, Nothing, Nothing]
            [0,       0,    0xAB,  0,       0      ]
    H.assert (Just (0x200, 0xAB) `elem` wr)

-- Done fires exactly once, on the cycle of the last write.
prop_dma_done_fires_once :: H.Property
prop_dma_done_fires_once = H.withTests 1 . H.property $ do
    let (_, _, _, done) = runDMA 6
            [Nothing, Just (0x100, 0x200, 1, True, True), Nothing, Nothing, Nothing, Nothing]
            [0,       0,    0xAB,  0,       0,       0     ]
    H.assert (length (filter (== True) done) == 1)

-- DMA is idle (not busy) after the transfer completes.
prop_dma_idle_after_done :: H.Property
prop_dma_idle_after_done = H.withTests 1 . H.property $ do
    let (_, _, busy, done) = runDMA 6
            [Nothing, Just (0x100, 0x200, 1, True, True), Nothing, Nothing, Nothing, Nothing]
            [0,       0,    0xAB,  0,       0,       0     ]
    let doneIdx = length (takeWhile (== False) done)
    H.assert (doneIdx < 6)
    H.assert (all (== False) (drop (doneIdx + 1) busy))

-- Multi-element transfer: 3 bytes copied.
--   cycle 1: trigger        → rdAddr=0x100
--   cycle 2: rdData=0xAA   → wr(0x200,0xAA), rdAddr=0x101
--   cycle 3: rdData=0xBB   → wr(0x201,0xBB), rdAddr=0x102
--   cycle 4: rdData=0xCC   → wr(0x202,0xCC), done
prop_dma_multi_element_transfer :: H.Property
prop_dma_multi_element_transfer = H.withTests 1 . H.property $ do
    let (rd, wr, _, done) = runDMA 8
            [Nothing, Just (0x100, 0x200, 3, True, True), Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
            [0,       0,    0xAA,  0xBB,  0xCC,  0,       0,       0      ]
    H.assert (Just 0x100 `elem` rd)
    H.assert (Just 0x101 `elem` rd)
    H.assert (Just 0x102 `elem` rd)
    H.assert (Just (0x200, 0xAA) `elem` wr)
    H.assert (Just (0x201, 0xBB) `elem` wr)
    H.assert (Just (0x202, 0xCC) `elem` wr)
    H.assert (length (filter (== True) done) == 1)

-- Memory-to-peripheral mode: dst is fixed, src increments.
prop_dma_m2p_dst_fixed :: H.Property
prop_dma_m2p_dst_fixed = H.withTests 1 . H.property $ do
    let (_, wr, _, _) = runDMA 8
            [Nothing, Just (0x100, 0x40, 3, True, False), Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
            [0,       0,    0xAA,  0xBB,  0xCC,  0,       0,       0      ]
    H.assert (Just (0x40, 0xAA) `elem` wr)
    H.assert (Just (0x40, 0xBB) `elem` wr)
    H.assert (Just (0x40, 0xCC) `elem` wr)

-- Peripheral-to-memory mode: src is fixed, dst increments.
prop_dma_p2m_src_fixed :: H.Property
prop_dma_p2m_src_fixed = H.withTests 1 . H.property $ do
    let (rd, wr, _, _) = runDMA 8
            [Nothing, Just (0x40, 0x200, 3, False, True), Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
            [0,       0,    0xDD,  0xEE,  0xFF,  0,       0,       0      ]
    let rdAddrs = [ a | Just a <- rd ]
    H.assert (all (== 0x40) rdAddrs)
    H.assert (Just (0x200, 0xDD) `elem` wr)
    H.assert (Just (0x201, 0xEE) `elem` wr)
    H.assert (Just (0x202, 0xFF) `elem` wr)

-- busMasterMux: CPU signals pass through when DMA is idle.
prop_bus_mux_cpu_when_idle :: H.Property
prop_bus_mux_cpu_when_idle = H.withTests 1 . H.property $ do
    let busy   = C.fromList [False, False, False] :: C.Signal C.System Bool
        cpuRd  = C.fromList [Just (0x10 :: Addr), Nothing, Just 0x20]
        cpuWr  = C.fromList [Nothing, Just (0x30 :: Addr, 0xAB :: Dat), Nothing]
        dmaRd  = C.pure Nothing
        dmaWr  = C.pure Nothing
        (effRd, effWr) = busMasterMux busy cpuRd cpuWr dmaRd dmaWr
    H.assert (C.sampleN 3 effRd == [Just 0x10, Nothing, Just 0x20])
    H.assert (C.sampleN 3 effWr == [Nothing, Just (0x30, 0xAB), Nothing])

-- busMasterMux: DMA signals override CPU when busy.
prop_bus_mux_dma_when_busy :: H.Property
prop_bus_mux_dma_when_busy = H.withTests 1 . H.property $ do
    let busy   = C.fromList [True, True, False] :: C.Signal C.System Bool
        cpuRd  = C.pure (Just (0x99 :: Addr))
        cpuWr  = C.pure (Just (0x99 :: Addr, 0xFF :: Dat))
        dmaRd  = C.fromList [Just 0x10, Just 0x11, Nothing]
        dmaWr  = C.fromList [Nothing, Just (0x20 :: Addr, 0xAB :: Dat), Nothing]
        (effRd, effWr) = busMasterMux busy cpuRd cpuWr dmaRd dmaWr
    H.assert (C.sampleN 3 effRd == [Just 0x10, Just 0x11, Just 0x99])
    H.assert (C.sampleN 3 effWr == [Nothing, Just (0x20, 0xAB), Just (0x99, 0xFF)])

dmaTests :: TestTree
dmaTests = $(testGroupGenerator)
