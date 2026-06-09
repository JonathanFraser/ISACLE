module Core.Periph.Timer
    ( TimerRegs(..)
    , timerUnit
    ) where

import Clash.Prelude

-- | Architectural registers exposed on the memory-mapped bus.
--
--   Register layout relative to base address:
--     base + 0  TCCR  read/write  control (bit 0 = CTC mode enable)
--     base + 1  TCNT  read/write  counter value
--     base + 2  OCR   read/write  output compare register
data TimerRegs dat = TimerRegs
    { tccr :: dat
    , tcnt :: dat
    , ocr  :: dat
    } deriving (Generic, NFDataX, Show, Eq)

-- | Generic memory-mapped timer/counter.
--
--   Counting is driven by the @tick@ input — a strobe that fires once per
--   desired count increment.  Pass @pure True@ for no prescaling; generate
--   @tick@ from an external prescaler for divided clocks.
--
--   Two operating modes selected by TCCR bit 0:
--     CTC mode (bit 0 = 1): counter resets to 0 when it reaches OCR.
--                           Compare-match interrupt fires on that cycle.
--     Normal mode (bit 0 = 0): counter wraps at @maxBound@.
--                              Overflow interrupt fires on wrap.
--
--   Parameterised over address (@addr@) and data (@dat@) types; @dat@ must
--   be @Bounded@ so wrapping / top-of-count detection is type-safe.
timerUnit
    :: forall dom addr dat
     . ( HiddenClockResetEnable dom
       , NFDataX dat, Num dat, Eq dat, Bounded dat, Bits dat
       , Eq addr, Num addr
       )
    => addr                                      -- base address
    -> Signal dom Bool                           -- tick (count enable)
    -> Signal dom (Maybe addr)                   -- bus read address
    -> Signal dom (Maybe (addr, dat))            -- bus write
    -> ( Signal dom dat                          -- read data
       , Signal dom Bool                         -- overflow interrupt
       , Signal dom Bool                         -- compare-match interrupt
       )
timerUnit base tick rdAddr wr = (rdData, ovfIrq, cmpIrq)
  where
    step regs (t, mrd, mwr) =
        let regs' = case mwr of
                Just (a, v)
                    | a == base     -> regs { tccr = v }
                    | a == base + 1 -> regs { tcnt = v }
                    | a == base + 2 -> regs { ocr  = v }
                _                   -> regs

            ctcMode = testBit (tccr regs') 0
            atTop   = tcnt regs' == ocr regs'
            atMax   = tcnt regs' == maxBound

            (tcnt'', ovf, cmp) = case (t, ctcMode) of
                (True, True)  | atTop -> (0,          False, True)
                (True, False) | atMax -> (0,          True,  False)
                (True, _)             -> (tcnt regs' + 1, False, False)
                (False, _)            -> (tcnt regs', False, False)

            regs'' = regs' { tcnt = tcnt'' }

            rd = case mrd of
                Just a
                    | a == base     -> tccr regs''
                    | a == base + 1 -> tcnt regs''
                    | a == base + 2 -> ocr  regs''
                _                   -> 0

        in (regs'', (rd, ovf, cmp))

    out    = mealy step (TimerRegs 0 0 0) (bundle (tick, rdAddr, wr))
    rdData = fmap (\(r, _, _) -> r) out
    ovfIrq = fmap (\(_, o, _) -> o) out
    cmpIrq = fmap (\(_, _, c) -> c) out
