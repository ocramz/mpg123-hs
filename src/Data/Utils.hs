module Data.Utils where

import Data.Bits
import Data.Int (Int8)  -- CChar
import Data.Word (Word8)  -- CUChar

wi8 :: Word8 -> Int8
wi8 = fromIntegral

iw8 :: Int8 -> Word8
iw8 = fromIntegral
