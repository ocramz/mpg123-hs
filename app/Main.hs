-- {-# LANGUAGE QuasiQuotes #-}
-- {-# LANGUAGE TemplateHaskell #-}
module Main where

import Options.Applicative
import Data.Semigroup ((<>))

-- import qualified Language.C.Inline as C

-- C.include "<stdio.h>"
-- C.include "<math.h>"


main = putStrLn "hello!"


-- main :: IO ()
-- main = do
--    x <- [C.exp| int{ printf("Some number: %.2f\n", cos(0.5)) } |]
--    putStrLn $ show x ++ " characters printed."




data Options 
