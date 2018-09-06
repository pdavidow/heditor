module Main where

import Scanner (scanFile)

main :: IO ()
main = do
    putStrLn "Enter Output file-path"
    output <- getLine
    scanFile "test/input/test_input_good_1.txt" output
