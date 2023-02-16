module Main where

import Control.Monad
import System.IO
import Test.Tasty.Bench
import qualified Control.Exception as E
import qualified Data.ByteString as BS

main :: IO ()
main = do
  E.try @IOError (withRandomBytes $ \_ -> pure ()) >>= \case
    Left e -> do
      putStrLn "Generate random_bytes with \"head -c 1G /dev/urandom > random_bytes\""
      E.throwIO e
    Right () -> defaultMain
      [ bgroup "read" $ map readBench [2*kB, 4*kB, 8*kB, 16*kB, 32*kB, 64*kB]
      ]

readBench :: Int -> Benchmark
readBench chunkSize = bench (show chunkSize) $
  nfAppIO (withRandomBytes . chunkedRead) chunkSize

----------------------------------------

withRandomBytes :: (Handle -> IO r) -> IO r
withRandomBytes = withFile "random_bytes" ReadMode

kB :: Int
kB = 1024

chunkedRead :: Int -> Handle -> IO ()
chunkedRead chunkSize handle = do
  chunk <- BS.hGetSome handle chunkSize
  unless (BS.null chunk) $ do
    chunkedRead chunkSize handle
