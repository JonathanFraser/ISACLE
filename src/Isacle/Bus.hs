module Isacle.Bus
    ( busReadMux
    , busMasterMux
    ) where

import Clash.Prelude

-- | Combine read-data outputs from N memory-mapped peripherals by OR-fold.
--   Valid when address ranges are non-overlapping: each peripheral returns 0
--   for addresses outside its range, so at most one non-zero response exists
--   per cycle and OR correctly selects it.
busReadMux
    :: (KnownNat n, Num dat, Bits dat)
    => Vec n (Signal dom dat)
    -> Signal dom dat
busReadMux = foldl (liftA2 (.|.)) (pure 0)

-- | Select between two bus masters (e.g. CPU and DMA).
--   When @busy@ is True the DMA signals are routed to the bus;
--   otherwise the CPU signals are routed.
busMasterMux
    :: Signal dom Bool                        -- DMA busy
    -> Signal dom (Maybe addr)                -- CPU read address
    -> Signal dom (Maybe (addr, dat))         -- CPU write
    -> Signal dom (Maybe addr)                -- DMA read address
    -> Signal dom (Maybe (addr, dat))         -- DMA write
    -> ( Signal dom (Maybe addr)
       , Signal dom (Maybe (addr, dat))
       )
busMasterMux busy cpuRd cpuWr dmaRd dmaWr =
    (mux busy dmaRd cpuRd, mux busy dmaWr cpuWr)
