module Main where

import Scanner (scanFile)

main :: IO ()
main = do
    result <- scanFile "test/input/test_input_good_1.txt" 
    case result of
        Left err ->
            putStrLn $ "ERROR: " ++ err

        Right output ->
            putStrLn output
