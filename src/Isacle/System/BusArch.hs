-- NB: NoImplicitPrelude is active from cabal common-options.
module Isacle.System.BusArch
    ( BusArch
    , SimpleBus(..)
    ) where

-- | Marks a type as a bus architecture.
--
-- This class is intentionally empty: it acts as a constraint that ensures
-- only declared architectures are used with 'Isacle.System.BusDef.Bus'.
-- Protocol-specific methods (interconnect construction, stall handling,
-- arbitration) will be added here when concrete bus implementations
-- beyond 'SimpleBus' are introduced.
--
-- A system may have multiple buses with different architectures; the
-- architecture is a phantom type parameter on 'Isacle.System.BusDef.Bus'
-- rather than a constraint on 'Isacle.System.Builder.SystemDSL'.
class BusArch arch

-- | A simple synchronous memory-mapped bus.
--
-- Single master, byte-wide data, combinational address decode, no stalling,
-- no burst support.  Suitable for small AVR-style SoC designs.
data SimpleBus = SimpleBus

instance BusArch SimpleBus
