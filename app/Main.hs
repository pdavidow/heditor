module Main where

import Scanner (scanFile)

main :: IO ()
main = 
    scanFile "test/input/test_input_good_1.txt" "output.txt"
