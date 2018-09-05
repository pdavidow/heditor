module Scanner
    ( scanFile
    )
    where

import Safe (headMay, tailMay) 
import Text.Read (readMaybe)


newtype OpAppend = OpAppend String deriving (Eq, Show)
newtype OpDelete = OpDelete Int deriving (Eq, Show)
newtype OpPrint = OpPrint Int deriving (Eq, Show)
data OpUndo = OpUndo deriving (Eq, Show)
newtype OpUndoAppend = OpUndoAppend Int deriving (Eq, Show)
newtype OpUndoDelete = OpUndoDelete String deriving (Eq, Show)

data Tagged_Operation
    = Tagged_OpAppend OpAppend 
    | Tagged_OpDelete OpDelete 
    | Tagged_OpPrint OpPrint 
    | Tagged_OpUndo OpUndo
    | Tagged_OpUndoAppend OpUndoAppend
    | Tagged_OpUndoDelete OpUndoDelete

data Model_Undo
    = Model_OpUndoAppend OpUndoAppend
    | Model_OpUndoDelete OpUndoDelete

type Line = String      

data Model = Model
    { _string :: String
    , _undos :: [Model_Undo]
    , _appendageLengthSum :: Int
    , _charDeleteCountSum :: Int
    }

    
initialModel :: Model
initialModel = 
    Model "" [] 0 0


opcodeAppend = 1 :: Int
opcodeDelete = 2 :: Int
opcodePrint  = 3 :: Int
opcodeUndo   = 4 :: Int


scanFile :: FilePath -> FilePath -> IO ()
scanFile input output = do
    lines <- getLines input
    case headMay lines of
        Just h -> do
            let mbOpCount = readMaybe h :: Maybe Int
            case mbOpCount of
                Just opCount ->
                    case tailMay lines of
                        Just opLines -> do
                            let eiResult = parseOps opCount opLines
                            case eiResult of
                                Right result ->
                                    --writeTo output result
                                    pure ()
                                Left err ->
                                    putStrLn err
                        Nothing -> do
                            putStrLn $ errorWithLineNum 2 "Operation expected" 
                Nothing -> 
                    putStrLn $ errorWithLineNum 1 "Operation count expected" 
        Nothing -> do
            putStrLn $ errorWithLineNum 1 "Operation count expected" 
    

getLines :: FilePath -> IO [Line]
getLines x = do
    -- https://stackoverflow.com/questions/12288318/read-a-file-line-by-line
    contents <- readFile x -- lazy
    pure $ lines contents    


parseOps :: Int -> [Line] -> Either String String
parseOps opCount xs = do
    case opCount == length xs of
        True -> do
            let numberedLines = zip [(2 :: Int)..] xs 
            let eiOps = parseCleanOps [] numberedLines

            case eiOps of
                Right ops ->
                    Right $ _string $ performOps initialModel ops

                Left err -> do
                    Left err

        False -> do
            Left "Operation count (from line 1) does not match actual"          


parseCleanOps :: [Tagged_Operation] -> [(Int, Line)] -> Either String [Tagged_Operation]
parseCleanOps acc [] = 
    Right acc
parseCleanOps acc (numberedLine : xs) = 
    let 
        eiOp = parseOp numberedLine
    in
        case eiOp of
            Right op ->
                parseCleanOps (acc ++ [op]) xs
            Left _ -> 
                eiOp
    

performOps :: Model -> [Tagged_Operation] -> Model
performOps model ops =
    model


performOp numberedLine = do
    let eiTaggedOp = parseOp numberedLine
    case eiTaggedOp of
        Right taggedOp ->

            pure ()

        Left err ->
            putStrLn err


-- todo refactor
parseOp :: (Int, Line) -> Either String Tagged_Operation
parseOp (n, line) = do
    let tokens = words line
    let tokenLength = length tokens
    case tokenLength > 0 of
        True -> do
            let mbOpCode = readMaybe $ head tokens :: Maybe Int -- safe head
            case mbOpCode of
                Just opCode ->
                    case opCode of
                        opcodeAppend ->
                            case tailMay tokens of
                                Just args ->
                                    case length args == 1 of
                                        True -> do
                                            let mbArg = readMaybe $ head args :: Maybe String -- safe head
                                            case mbArg of
                                                Just arg -> 
                                                    Right $ Tagged_OpAppend $ OpAppend arg
                                                Nothing -> 
                                                    Left $ errorWithLineNum n "Append has one string arg"
                                        False ->
                                            Left $ errorWithLineNum n "Append has one arg"  
                                Nothing ->
                                    Left $ errorWithLineNum n "Append has one arg"   
                        opcodeDelete ->
                            case tailMay tokens of
                                Just args ->
                                    case length args == 1 of
                                        True -> do
                                            let mbArg = readMaybe $ head args :: Maybe Int -- safe head
                                            case mbArg of
                                                Just arg -> 
                                                    Right $ Tagged_OpDelete $ OpDelete arg
                                                Nothing -> 
                                                    Left $ errorWithLineNum n "Delete has one int arg"
                                        False ->
                                            Left $ errorWithLineNum n "Delete has one arg"  
                                Nothing ->
                                    Left $ errorWithLineNum n "Delete has one arg"                             
                        opcodePrint ->
                            case tailMay tokens of
                                Just args ->
                                    case length args == 1 of
                                        True -> do
                                            let mbArg = readMaybe $ head args :: Maybe Int -- safe head
                                            case mbArg of
                                                Just arg -> 
                                                    Right $ Tagged_OpPrint $ OpPrint arg
                                                Nothing -> 
                                                    Left $ errorWithLineNum n "Print has one int arg"
                                        False ->
                                            Left $ errorWithLineNum n "Print has one arg"  
                                Nothing ->
                                    Left $ errorWithLineNum n "Print has one arg"                               
                        opcodeUndo ->
                            case tokenLength == 1 of
                                True ->
                                    Right $ Tagged_OpUndo $ OpUndo
                                False ->
                                    Left $ errorWithLineNum n "Undo has no args"    
                        _ -> 
                            Left $ errorWithLineNum n "Invalid operation type"     
                Nothing ->
                    Left $ errorWithLineNum n "Invalid operation type"     
        False ->
            Left $ errorWithLineNum n "Operation type expected"     


errorWithLineNum :: Int -> String -> String
errorWithLineNum lineNum error =
    error ++ ", line " ++ show lineNum


-- makeOp :: Int -> [String] -> IO (Either String Operation)
-- makeOp opCode tokens = do
--     let argType = case
