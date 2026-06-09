module Core.Harvard.CPU where

import Clash.Prelude
import Core.ALU (ALU(..))
import Data.Maybe (fromMaybe)

-- | Execute one decoded instruction using an 'ALU', advancing the program counter.
--
--   The write address is computed from the PRE-compute state so that
--   post-increment/pre-decrement pointer updates in 'compute' do not
--   corrupt the write destination (matching the convention in 'Core.ALU').
--
--   Returns @(new_state, maybe_write, next_pc)@.
runInstruction
    :: Num romaddr
    => ALU instr state ramaddr romaddr val
    -> (state -> romaddr)             -- getPC
    -> (state -> romaddr -> state)    -- setPC
    -> (instr -> romaddr)             -- instruction size in fetch words
    -> instr
    -> Maybe val
    -> state
    -> (state, Maybe (ramaddr, val), romaddr)
runInstruction alu getPC setPC instrSize instr mval s =
    let writeSpec = write alu instr s
        s'        = compute alu instr mval s
        nextPC    = fromMaybe (getPC s + instrSize instr) (jump alu instr s')
    in (setPC s' nextPC, writeSpec, nextPC)
