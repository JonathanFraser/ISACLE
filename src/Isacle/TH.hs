-- NB: NoImplicitPrelude is active from cabal common-options; re-import
-- the standard Prelude explicitly so this TH module can use plain IO / Bits.
module Isacle.TH
    ( loadBinWith
    , loadBin8
    , loadBin16LE
    , loadBin32LE
    , padToPow2
    , nextPow2
    ) where

import Prelude
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addDependentFile)
import qualified Data.ByteString as BS
import Data.Bits  (shiftL, (.|.), countLeadingZeros, finiteBitSize, bit)
import Data.Word  (Word8)

import Clash.Prelude (listToVecTH)

-- | Read a flat binary at compile time and splice it as @Vec n (BitVector w)@.
--
--   The @parser@ function converts the raw byte stream to a list of
--   'Integer' values, one per vector element.  The list is then zero-padded
--   to the next power of two so the resulting 'Vec' size is always a power of
--   two — required for efficient block-RAM addressing.
--
--   The file path is relative to the project root (where GHC is invoked).
--   GHC recompiles the module whenever the file changes because
--   'addDependentFile' registers it as a dependency.
loadBinWith :: ([Word8] -> [Integer]) -> FilePath -> Q Exp
loadBinWith parser path = do
    addDependentFile path
    content <- runIO (BS.readFile path)
    listToVecTH (padToPow2 (parser (BS.unpack content)))

-- | Load a byte-addressed binary: each byte becomes one vector element.
--   Suitable for 8-bit ROM images (MCS-51, 6502, Z80, etc.).
loadBin8 :: FilePath -> Q Exp
loadBin8 = loadBinWith (map fromIntegral)

-- | Load a little-endian 16-bit binary: each pair of bytes (lo then hi)
--   becomes one vector element.
--   Suitable for 16-bit word-addressed ROMs (AVR, 8086, etc.).
loadBin16LE :: FilePath -> Q Exp
loadBin16LE = loadBinWith parseWords16LE

-- | Load a little-endian 32-bit binary: each group of four bytes becomes
--   one vector element.
--   Suitable for 32-bit word-addressed ROMs (RISC-V, ARM, MIPS, etc.).
loadBin32LE :: FilePath -> Q Exp
loadBin32LE = loadBinWith parseWords32LE

-- ---------------------------------------------------------------------------
-- Byte parsers
-- ---------------------------------------------------------------------------

parseWords16LE :: [Word8] -> [Integer]
parseWords16LE []           = []
parseWords16LE [_]          = []   -- trailing odd byte ignored
parseWords16LE (lo:hi:rest) =
    ((fromIntegral hi `shiftL` 8) .|. fromIntegral lo) : parseWords16LE rest

parseWords32LE :: [Word8] -> [Integer]
parseWords32LE (b0:b1:b2:b3:rest) =
    ( fromIntegral b0
      .|. (fromIntegral b1 `shiftL`  8)
      .|. (fromIntegral b2 `shiftL` 16)
      .|. (fromIntegral b3 `shiftL` 24)
    ) : parseWords32LE rest
parseWords32LE _ = []   -- trailing partial word ignored

-- ---------------------------------------------------------------------------
-- Padding utilities (exported for use in other TH helpers)
-- ---------------------------------------------------------------------------

-- | Pad a list to the next power of two with zeros (or a single 0 if empty).
--   A power-of-two size means the ROM index is a clean bit-truncation of the
--   program counter — no modulo required at synthesis time.
padToPow2 :: [Integer] -> [Integer]
padToPow2 [] = [0]
padToPow2 xs =
    let n  = length xs
        n' = nextPow2 n
    in xs ++ replicate (n' - n) 0

-- | Smallest power of two >= n.
nextPow2 :: Int -> Int
nextPow2 n
    | n <= 1    = 1
    | otherwise = bit k
  where
    k = finiteBitSize (0 :: Int) - countLeadingZeros (n - 1)
