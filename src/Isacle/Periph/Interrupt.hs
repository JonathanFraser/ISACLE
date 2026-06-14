module Isacle.Periph.Interrupt
    ( interruptArbiter
    ) where

import Clash.Prelude

-- | Combinational priority interrupt arbiter.
--
--   Sources are in priority order: index 0 = highest priority.  When multiple
--   sources are active simultaneously the lowest-index request wins.
--
--   The output is gated by the caller-supplied @iEnabled@ signal (global
--   interrupt enable flag).  If @iEnabled@ is False the output is always
--   Nothing regardless of active requests.
--
--   Parameterised over @addr@ so the same arbiter works for any ISA's
--   interrupt vector address type (AVR word addresses, 8051 byte addresses, etc.).
--
--   For use inside 'SystemDSL' descriptions prefer 'Isacle.System.Builder.addIrq',
--   which registers sources individually and avoids polymorphism issues with
--   the Clash elaborator.
interruptArbiter
    :: KnownNat n
    => Vec n (Signal dom Bool, addr)   -- ^ (request line, vector address), index 0 = highest priority
    -> Signal dom Bool                 -- ^ global interrupt enable
    -> Signal dom (Maybe addr)
interruptArbiter sources iEnabled = liftA2 gate iEnabled winner
  where
    candidates = map (\(req, vec) -> fmap (\r -> if r then Just vec else Nothing) req) sources
    winner     = foldr (liftA2 firstJust) (pure Nothing) candidates

    firstJust (Just a) _ = Just a
    firstJust Nothing  b = b

    gate True  w = w
    gate False _ = Nothing
