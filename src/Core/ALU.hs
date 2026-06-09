module Core.ALU where

import Data.Maybe (Maybe)

-- | Generic CPU ALU interface.
--
--   Each field is a pure function from instruction + state to effect:
--     read    — which data RAM address to read before compute (if any)
--     compute — how the instruction transforms the CPU state
--     write   — which data RAM address/value to write after compute (if any)
--     jump    — where the program counter goes next (Nothing = sequential)
--
--   The four-way split matches the structural stages of most load-store CPUs
--   and is the key composition point: swap any field independently to build a
--   different CPU variant without touching the pipeline.
data ALU instr state ramaddr romaddr val = ALU
    { read    :: instr -> state -> Maybe ramaddr
    , compute :: instr -> Maybe val -> state -> state
    , write   :: instr -> state -> Maybe (ramaddr, val)
    , jump    :: instr -> state -> Maybe romaddr
    }
