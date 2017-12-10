import Codec.Mpg123.Internal

main :: IO ()
main = withMpg123 $ \_ -> do
  putStrLn =<< mpg123decoder
