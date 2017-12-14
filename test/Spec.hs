module Spec where

import Codec.Mpg123.Internal

-- import Test.Hspec
import Test.HUnit


main :: IO Counts
main = runTestTT $ TestList [t1]

t1 :: Test
t1 = TestLabel "mpg123decoder == AVX" $ TestCase $ withMpg123 $ \_ -> do
       s <- mpg123decoder
       s @=? "AVX"


