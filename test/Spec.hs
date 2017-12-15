module Main where

import Codec.Mpg123.Internal

-- import Test.Hspec
import Test.HUnit


-- main :: IO Counts
main = do
  -- print "hello!"
  print =<< mpg123decoders 4
  -- runTestTT $ TestList [t1]


-- t1 :: Test
-- t1 = TestLabel "mpg123decoder == AVX" $ TestCase $ withMpg123 $ \_ -> do
--        s <- mpg123decoders
--        s @=? "\nAVX,x86-64,generic,generic_dither"


