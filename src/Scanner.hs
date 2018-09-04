module Scanner
    ( scanFile
    )
    where

scanFile :: FilePath -> IO Int  
scanFile x = do
    contents <- readFile x
    pure $ read contents