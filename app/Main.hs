-- {-# LANGUAGE QuasiQuotes #-}
-- {-# LANGUAGE TemplateHaskell #-}
module Main where

import Options.Applicative
import Data.Semigroup ((<>))

import Codec.Mpg123.Internal -- (decode, readWriteHdl)

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

runner (Options bo fi fo ) = do
  -- putStrLn $ unwords [show bo, show fi, show fo]
  decode fi fo (fromIntegral bo)
  -- readWriteHdl fi fo

bufSizeOutDefault :: Int
bufSizeOutDefault = 2^15

data Options = Options {
    bufSizeOut :: Int
  , fileIn :: FilePath
  , fileOut :: FilePath
                        } deriving (Eq, Show)

-- optVerbose 

options :: Parser Options
options = Options
  <$> option auto (
       long "bufSizeOut"
    <> short 'b'
    <> help "Output buffer size"
    <> showDefault
    <> value bufSizeOutDefault <> metavar "BYTES")
  <*> strOption (
       long "fileIn"
    <> short 'i'
    <> help "Input file path" <> metavar "PATH")
  <*> strOption (
       long "fileOut"
    <> short 'o'
    <> help "Output file path" <> metavar "PATH")

bufSizeOpt :: Parser Int
bufSizeOpt = option auto (
       long "bufSizeOut"
    <> short 'b'
    <> help "Output buffer size"
    <> showDefault
    <> value bufSizeOutDefault
    <> metavar "BYTES")

fileInOpt :: Parser String
fileInOpt = strOption (
       long "fileIn"
    <> short 'i'
    <> help "Input file path"
    <> metavar "PATH")

fileOutOpt :: Parser String
fileOutOpt = strOption (
       long "fileOut"
    <> short 'o'
    <> help "Output file path"
    <> metavar "PATH")            



data VerboseOption = SilentOpt | VerboseOpt deriving (Eq, Show)

asdf = flag' VerboseOpt
