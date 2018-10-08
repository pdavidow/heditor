module Main where

import Scanner (scanFile)

main :: IO ()
main = do
    let input = "test/input/test_input_good_1.txt" 
    let output = "output_1.txt"
    result <- scanFile input output 
    
    case result of
        Left err ->
            putStrLn $ "ERROR: " ++ err

        Right _ ->
            pure ()
