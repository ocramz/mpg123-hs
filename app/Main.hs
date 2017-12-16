-- {-# LANGUAGE QuasiQuotes #-}
-- {-# LANGUAGE TemplateHaskell #-}
module Main where

import Options.Applicative
import Data.Semigroup ((<>))

import Codec.Mpg123.Internal (decode, readWriteHdl)

-- import qualified Language.C.Inline as C

-- C.include "<stdio.h>"
-- C.include "<math.h>"


-- main = putStrLn "hello!"


-- main :: IO ()
-- main = do
--    x <- [C.exp| int{ printf("Some number: %.2f\n", cos(0.5)) } |]
--    putStrLn $ show x ++ " characters printed."



main :: IO ()
main = runner =<< execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc
     -- <> progDesc "Print a greeting for TARGET"
     -- <> header "Test program for mpg123"
      )

-- greet :: Sample -> IO ()
-- greet (Sample h False n) = putStrLn $ "Hello, " ++ h ++ replicate n '!'
-- greet _ = return ()

runner (Options _ fi fo ) = do
  -- putStrLn $ unwords [show bi, show bo, show fi, show fo]
  -- decode fi fo (fromIntegral bo)
  readWriteHdl fi fo


bufSizeInDefault = 2^14
bufSizeOutDefault = 2^15

data Options = Options {
    bufSizeOut :: Int
  , fileIn :: FilePath
  , fileOut :: FilePath
                        } deriving (Eq, Show)

options :: Parser Options
options = Options
  <$> option auto (
       long "bufSizeOut"
    <> short 'b'
    <> help "Output buffer size [bytes]"
    <> showDefault
    <> value bufSizeOutDefault )
  <*> strOption (
       long "fileIn"
    <> short 'i'
    <> help "Input file path")
  <*> strOption (
       long "fileOut"
    <> short 'o'
    <> help "Output file path")



-- sample :: Parser Sample
-- sample = Sample
--       <$> strOption
--           ( long "hello"
--          <> metavar "TARGET"
--          <> help "Target for the greeting" )
--       <*> switch
--           ( long "quiet"
--          <> short 'q'
--          <> help "Whether to be quiet" )
--       <*> option auto
--           ( long "enthusiasm"
--          <> help "How enthusiastically to greet"
--          <> showDefault
--          <> value 1
--          <> metavar "INT" )
