module Main where

import Control.Concurrent
import Control.Concurrent.Async

main = do
          asyncRes <- async $ mapConcurrently (putStrLn . show) [1..10000]
          putStrLn "computing.."
          let loop = do
              maybeResults <- poll asyncRes
              case maybeResults of
                  Nothing -> do
                      putStrLn "still computing..."
                      threadDelay 1000
                      loop
                  Just r -> return r

          loop >>= print
