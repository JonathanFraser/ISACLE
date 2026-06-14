module Isacle.Periph.DMA
    ( DMAState(..)
    , dmaEngine
    ) where

import Clash.Prelude

-- | DMA transfer engine state.
--
--   'DMARead' means a read was issued to @src@ in the previous cycle; this
--   cycle the read response arrives and is written to @dst@, then either the
--   next read is issued (if count > 1) or the transfer completes.
data DMAState addr dat
    = DMAIdle
    | DMARead addr addr (Unsigned 16) Bool Bool
      -- src dst count incSrc incDst
    deriving (Generic, NFDataX, Show, Eq)

-- | Single-channel DMA transfer engine.
--
--   Performs autonomous block transfers between bus addresses without CPU
--   involvement.  Presents the same @(Maybe addr, Maybe (addr, dat))@ bus
--   interface as CPU bus masters; use 'Isacle.Bus.busMasterMux' to arbitrate.
--
--   Transfer modes are controlled by @incSrc@ / @incDst@ in the start signal:
--     True  / True  — memory-to-memory (both addresses increment each step)
--     True  / False — memory-to-peripheral (src increments, dst is fixed)
--     False / True  — peripheral-to-memory (src is fixed, dst increments)
--
--   Timing: a transfer of @n@ elements takes @n + 1@ clock cycles.
--     Cycle 0     : DMA issues first read; bus is taken (busy = True)
--     Cycles 1…n-1: simultaneous write of previous data + read of next element
--     Cycle n     : final write committed; done fires; bus released next cycle
--
--   The @start@ signal is sampled every cycle; a new transfer is accepted only
--   when the DMA is idle (DMAIdle).  Zero-count starts are ignored.
dmaEngine
    :: forall dom addr dat
     . ( HiddenClockResetEnable dom
       , NFDataX addr, Num addr
       , NFDataX dat
       )
    => Signal dom (Maybe (addr, addr, Unsigned 16, Bool, Bool))
       -- ^ start: (src, dst, count, incSrc, incDst) — Nothing = no new transfer
    -> Signal dom dat
       -- ^ bus read response (one cycle after the read address was presented)
    -> ( Signal dom (Maybe addr)              -- DMA read address
       , Signal dom (Maybe (addr, dat))       -- DMA write
       , Signal dom Bool                      -- busy (holds True through last write)
       , Signal dom Bool                      -- done (pulses True on last write cycle)
       )
dmaEngine start rdData = (dmaRd, dmaWr, busy, done)
  where
    step DMAIdle (Just (src, dst, n, iSrc, iDst), _)
        | n > 0
        = (DMARead src dst n iSrc iDst, (Just src, Nothing, True, False))
    step DMAIdle _
        = (DMAIdle, (Nothing, Nothing, False, False))

    step (DMARead src dst n iSrc iDst) (_, dat) =
        let nextSrc = if iSrc then src + 1 else src
            nextDst = if iDst then dst + 1 else dst
            n'      = n - 1
            isDone  = n' == 0
            nextSt  = if isDone then DMAIdle
                                else DMARead nextSrc nextDst n' iSrc iDst
            nextRd  = if isDone then Nothing else Just nextSrc
        in (nextSt, (nextRd, Just (dst, dat), True, isDone))

    out   = mealy step DMAIdle (bundle (start, rdData))
    dmaRd = fmap (\(rd, _, _, _) -> rd) out
    dmaWr = fmap (\(_, wr, _, _) -> wr) out
    busy  = fmap (\(_, _, b, _) -> b) out
    done  = fmap (\(_, _, _, d) -> d) out
