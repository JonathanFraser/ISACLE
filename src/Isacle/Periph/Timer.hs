module Isacle.Periph.Timer
    ( -- * Peripheral kind tag
      Timer
      -- * PeriphDef description (single source of truth)
    , timerDef
    , timerSpecPeriphDef
      -- * Physical I/O instance
      -- (instance HasPhysIO Timer)
      -- * Backward-compatible circuit wrapper
    , timerUnit
    ) where

import Clash.Prelude hiding (register)

import Isacle.System.Periph
import Isacle.System.Circuit
import Isacle.System.Spec (NullSig(..))

-- ---------------------------------------------------------------------------
-- Peripheral kind tag
-- ---------------------------------------------------------------------------

data Timer

-- ---------------------------------------------------------------------------
-- Register map — single source of truth
-- ---------------------------------------------------------------------------

-- | Timer register map.
--
--   offset 0  TCCR  read/write  control (bit 0 = CTC mode)
--   offset 1  TCNT  read/write  counter value (reads current counter)
--   offset 2  OCR   read/write  output compare register
--
-- @cntSig@ is the current counter value driven by the counter state machine.
-- Writing TCNT presets the counter; the write-first bypass is handled by
-- 'counterFSM', not by the register itself.
--
-- Returns @(tccr, ocr, tcntPreset, tcntWritten)@.
timerDef
    :: (Applicative sig, Num dat)
    => sig dat                     -- ^ current counter value (from counter FSM)
    -> PeriphDef Timer sig dat (sig dat, sig dat, sig dat, sig Bool)
timerDef cntSig = do
    register RW8 0 "TCCR" "Timer control"
        [ bitF ReadWrite 0 "CTC" "CTC mode: reset counter on compare match" ]
    tccr <- onWrite 0 0
    onRead 0 tccr

    field8 ReadWrite 1 "TCNT" "Counter value (write to preset)"
    (tcntPreset, tcntWritten) <- onWriteStrobe 1 0
    onRead 1 cntSig                 -- reads actual counter, not the preset register

    field8 ReadWrite 2 "OCR" "Output compare register"
    ocr <- onWrite 2 0
    onRead 2 ocr

    return (tccr, ocr, tcntPreset, tcntWritten)

-- | Documentation-only 'PeriphDef' for use with 'mkPeriph' in spec interpreters.
timerSpecPeriphDef :: (Applicative sig, Num dat) => PeriphDef Timer sig dat (PhysOutputs Timer sig)
timerSpecPeriphDef = timerDef (pure 0) >> return (pure False, pure False)

-- ---------------------------------------------------------------------------
-- Physical I/O instance
-- ---------------------------------------------------------------------------

instance HasPhysIO Timer where
    type PeriphSize  Timer     = 3   -- TCCR(0), TCNT(1), OCR(2)
    type PhysInputs  Timer sig = sig Bool           -- tick / count enable
    type PhysOutputs Timer sig = (sig Bool, sig Bool)  -- (overflow IRQ, compare IRQ)
    nullOutputs _ = (NullSig, NullSig)

-- ---------------------------------------------------------------------------
-- Counter state machine (synthesis only)
-- ---------------------------------------------------------------------------

-- | Clocked counter that implements the timer behaviour described by 'timerDef'.
--
-- The 'mealy' register breaks the mutual dependency between 'timerDef' and
-- the counter logic: @tccr@, @ocr@, and @tcntWritten@ are one cycle old from
-- the counter's perspective, which is identical to the previous hand-written
-- @mealy step (TimerRegs 0 0 0)@ implementation.
counterFSM
    :: ( HiddenClockResetEnable dom
       , NFDataX dat, Num dat, Eq dat, Bounded dat, Bits dat
       )
    => Signal dom Bool   -- ^ tick / count enable
    -> Signal dom dat    -- ^ tccr (from timerDef)
    -> Signal dom dat    -- ^ ocr  (from timerDef)
    -> Signal dom dat    -- ^ tcntPreset (from timerDef onWriteStrobe)
    -> Signal dom Bool   -- ^ tcntWritten (write strobe from timerDef)
    -> (Signal dom dat, Signal dom Bool, Signal dom Bool)  -- ^ (cnt, ovf, cmp)
counterFSM tick tccr ocr tcntPreset tcntWritten =
    let step cnt (t, ctrl, cmp_val, preset, written) =
            let ctcMode = testBit ctrl 0
                atTop   = cnt == cmp_val
                atMax   = cnt == maxBound

                cnt' = if written then preset
                       else case (t, ctcMode) of
                                (True, True)  | atTop -> 0
                                (True, False) | atMax -> 0
                                (True, _)             -> cnt + 1
                                (False, _)            -> cnt

                ovf = t && not ctcMode && atMax && not written
                cmp = t && ctcMode     && atTop && not written

            -- Output cnt' (post-write, post-tick) so same-cycle reads see
            -- the updated value — matches the behaviour of the original mealy.
            in (cnt', (cnt', ovf, cmp))

        out    = mealy step 0 (bundle (tick, tccr, ocr, tcntPreset, tcntWritten))
        cntSig = fmap (\(c, _, _) -> c) out
        ovfSig = fmap (\(_, o, _) -> o) out
        cmpSig = fmap (\(_, _, c) -> c) out
    in (cntSig, ovfSig, cmpSig)

-- ---------------------------------------------------------------------------
-- Backward-compatible circuit wrapper
-- ---------------------------------------------------------------------------

-- | Generic memory-mapped timer/counter, derived from 'timerDef'.
--
--   Register layout:
--     base + 0  TCCR  control
--     base + 1  TCNT  counter (write = preset, read = current value)
--     base + 2  OCR   output compare
timerUnit
    :: forall dom addr dat.
       ( HiddenClockResetEnable dom
       , Integral addr, Num addr, Eq addr
       , NFDataX dat, Num dat, Eq dat, Bounded dat, Bits dat
       )
    => addr                               -- ^ base address
    -> Signal dom Bool                    -- ^ tick / count enable
    -> Signal dom (Maybe addr)            -- ^ bus read address
    -> Signal dom (Maybe (addr, dat))     -- ^ bus write
    -> ( Signal dom dat                   -- ^ read data
       , Signal dom Bool                  -- ^ overflow interrupt
       , Signal dom Bool                  -- ^ compare-match interrupt
       )
timerUnit base tick rdAddr wr = (rdData, ovfIrq, cmpIrq)
  where
    -- Mutual recursion: cntSig feeds timerDef, timerDef feeds counterFSM,
    -- counterFSM produces cntSig.  The mealy register in counterFSM breaks
    -- the combinational cycle.
    ((tccr, ocr, tcntPreset, tcntWritten), rdData) =
        runSynthPeriph base wr rdAddr (timerDef cntSig)

    (cntSig, ovfIrq, cmpIrq) =
        counterFSM tick tccr ocr tcntPreset tcntWritten
