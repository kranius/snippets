module Main where

main :: IO ()
main = do
    input <- getContents
    let cleaninput = map reverse $ tail $ lines input
    print cleaninput
