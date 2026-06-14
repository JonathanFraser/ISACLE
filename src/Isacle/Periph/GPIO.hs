module Isacle.Periph.GPIO
    ( -- * Peripheral kind tag
      GPIO
      -- * PeriphDef description (single source of truth)
    , gpioDef
      -- * Physical I/O instance
      -- (instance HasPhysIO GPIO)
      -- * Backward-compatible circuit wrapper
    , gpioUnit
    ) where

import Clash.Prelude
import Isacle.System.Periph
import Isacle.System.Circuit
import Isacle.System.Spec (NullSig(..))

-- ---------------------------------------------------------------------------
-- Peripheral kind tag
-- ---------------------------------------------------------------------------

data GPIO

-- ---------------------------------------------------------------------------
-- Register map description — single source of truth
-- ---------------------------------------------------------------------------

-- | GPIO register map.
--
--   offset 0  PIN   read-only   sampled physical inputs
--   offset 1  DDR   read/write  data direction (1 = output)
--   offset 2  PORT  read/write  output latch
--
-- @pinsIn@ is the current physical pin-input signal.
-- Returns @(PORT latch signal, DDR signal)@.
--
-- Generic over @dat@ so the same definition works for @BitVector 8@,
-- @Unsigned 8@, etc.
gpioDef
    :: (Applicative sig, Num dat)
    => sig dat                                    -- ^ physical pin inputs
    -> PeriphDef GPIO sig dat (sig dat, sig dat)  -- ^ (PORT output, DDR output)
gpioDef pinsIn = do
    field8 ReadOnly  0 "PIN"  "Sampled physical inputs"
    onRead 0 pinsIn

    field8 ReadWrite 1 "DDR"  "Data direction (1 = output)"
    ddr <- onWrite 1 0
    onRead 1 ddr

    field8 ReadWrite 2 "PORT" "Output latch"
    port <- onWrite 2 0
    onRead 2 port

    return (port, ddr)

-- ---------------------------------------------------------------------------
-- Physical I/O instance
-- ---------------------------------------------------------------------------

instance HasPhysIO GPIO where
    type PeriphSize  GPIO     = 3   -- PIN(0), DDR(1), PORT(2)
    type PhysInputs  GPIO sig = sig (BitVector 8)
    type PhysOutputs GPIO sig = (sig (BitVector 8), sig (BitVector 8))  -- (PORT, DDR)
    nullOutputs _ = (NullSig, NullSig)

-- ---------------------------------------------------------------------------
-- Backward-compatible circuit wrapper
-- ---------------------------------------------------------------------------

-- | Generic memory-mapped GPIO port with three consecutive registers.
--
--   Derived from 'gpioDef' via the synthesis runner; no hand-written
--   state machine.
--
--   Returns: @(read data, PORT output latch, DDR output-enable)@
gpioUnit
    :: forall dom addr dat.
       ( HiddenClockResetEnable dom
       , Integral addr, Num addr, Eq addr
       , NFDataX dat, Num dat
       )
    => addr                               -- ^ base address
    -> Signal dom dat                     -- ^ physical pin inputs
    -> Signal dom (Maybe addr)            -- ^ bus read address
    -> Signal dom (Maybe (addr, dat))     -- ^ bus write
    -> ( Signal dom dat                   -- ^ read data
       , Signal dom dat                   -- ^ PORT output latch
       , Signal dom dat                   -- ^ DDR output-enable
       )
gpioUnit base pinsIn rdAddr wr =
    let ((port, ddr), rdData) = runSynthPeriph base wr rdAddr (gpioDef pinsIn)
    in (rdData, port, ddr)
