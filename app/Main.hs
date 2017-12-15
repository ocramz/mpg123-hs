-- {-# LANGUAGE QuasiQuotes #-}
-- {-# LANGUAGE TemplateHaskell #-}
module Main where

import Options.Applicative
import Data.Semigroup ((<>))

-- import qualified Language.C.Inline as C

-- C.include "<stdio.h>"
-- C.include "<math.h>"


-- main = putStrLn "hello!"


-- main :: IO ()
-- main = do
--    x <- [C.exp| int{ printf("Some number: %.2f\n", cos(0.5)) } |]
--    putStrLn $ show x ++ " characters printed."



main :: IO ()
main = greet =<< execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc
     -- <> progDesc "Print a greeting for TARGET"
     -- <> header "hello - a test for optparse-applicative"
      )

-- greet :: Sample -> IO ()
-- greet (Sample h False n) = putStrLn $ "Hello, " ++ h ++ replicate n '!'
-- greet _ = return ()

greet (Options bi bo fi fo ) = putStrLn $ unwords [show bi, show bo, show fi, show fo]


bufSizeInDefault = 2^14
bufSizeOutDefault = 2^15

data Options = Options {
    bufSizeIn :: Int
  , bufSizeOut :: Int
  , fileIn :: FilePath
  , fileOut :: FilePath
                        } deriving (Eq, Show)

options :: Parser Options
options = Options
  <$> option auto (
       long "bufSizeIn"
    <> help "Input buffer size [bytes]"
    <> showDefault
    <> value bufSizeInDefault )
  <*> option auto (
       long "bufSizeOut"
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
