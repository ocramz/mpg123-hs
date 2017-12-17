module Data.Utils where

import Data.Bits
import Data.Int (Int8, Int16)  -- CChar
import Data.Word (Word8, Word16)  -- CUChar

{- |
Word8 range 0..255
Int8 range -128..127
-}

-- | Unsigned-to-signed 8 bit word conversion
wi8 :: Word8 -> Int8
wi8 = fromIntegral

-- | Signed-to-unsigned 8 bit word conversion
iw8 :: Int8 -> Word8
iw8 = fromIntegral


widenW8 :: Word8 -> Word16
widenW8 = fromIntegral

widenI8 :: Int8 -> Int16
widenI8 = fromIntegral


biasIW8 i =
  let i16 = widenI8 i
  in shiftL i16 8
