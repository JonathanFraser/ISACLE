-- | ISACLE three-level SoC composition DSL.
--
-- Levels:
--
--   1. "Isacle.System.Periph"  — peripheral register definitions ('PeriphDef',
--      'HasPhysIO')
--   2. "Isacle.System.Builder" — physical I/O binding and CPU wiring
--      ('SystemDSL', 'mkPeriph', 'simpleHarvardCore')
--   3. "Isacle.System.BusDef"  — address-space layout ('BusDef', 'attach')
--   4. "Isacle.System.Generate" — artifact generators (C header, linker script)
module Isacle.System
    ( module Isacle.System.Spec
    , module Isacle.System.Periph
    , module Isacle.System.BusDef
    , module Isacle.System.BusArch
    , module Isacle.System.Builder
    , module Isacle.System.Generate
    , module Isacle.System.Synth
    ) where

import Isacle.System.Spec
import Isacle.System.Periph
import Isacle.System.BusDef
import Isacle.System.BusArch
import Isacle.System.Builder
import Isacle.System.Generate
import Isacle.System.Synth
