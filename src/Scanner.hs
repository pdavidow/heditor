module Scanner
    ( scanFile
    )
    where
import Debug.Trace (trace, traceIO, traceM)
import Safe (atMay, headMay, tailMay) 
import Text.Read (readMaybe)
import Data.Text (dropEnd, pack, takeEnd, unpack)

newtype OpAppend = OpAppend String deriving (Eq, Show)
newtype OpDelete = OpDelete Int deriving (Eq, Show)
newtype OpPrint = OpPrint Int deriving (Eq, Show)
data OpUndo = OpUndo deriving (Eq, Show)
newtype OpUndoAppend = OpUndoAppend Int deriving (Eq, Show)
newtype OpUndoDelete = OpUndoDelete String deriving (Eq, Show)

data Tagged_Operation -- todo include lineNums to give better error msgs
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
    , _printOutput :: String
    }

    
initialModel :: Model
initialModel = 
    Model "" [] 0 0 ""


opCount_UpperLimit            = 1000000
appendageLengthSum_UpperLimit = 1000000
charDeleteCountSum_UpperLimit = 2000000


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
                            let eiModel = parseOps opCount opLines
                            case eiModel of
                                Right model -> do
                                    writeFile output $ _printOutput model
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


parseOps :: Int -> [Line] -> Either String Model
parseOps opCount xs = 
    case opCount == length xs of
        True -> 
            let 
                numberedLines = zip [(2 :: Int)..] xs 
                eiOps = parseCleanOps [] numberedLines
            in
                case eiOps of
                    Right ops ->
                        performOps initialModel ops

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
            Left err -> 
                Left err


performOps :: Model -> [Tagged_Operation] -> Either String Model
performOps model [] =
    Right model
performOps model (op : xs) =
    case op of
        Tagged_OpAppend (OpAppend appendage) ->
            let
                len = length appendage
                sum = _appendageLengthSum model + len
            in
                case sum <= appendageLengthSum_UpperLimit of
                    True ->
                        let
                            model' =
                                Model
                                    (_string model ++ appendage)
                                    (_undos model ++ [Model_OpUndoAppend $ OpUndoAppend len])
                                    sum
                                    (_charDeleteCountSum model)
                                    (_printOutput model)
                        in
                            performOps model' xs

                    False ->
                        Left $ "The sum of the lengths of all appendage arguments (for Append) must be <= " ++ show appendageLengthSum_UpperLimit ++ ", but instead is " ++ show sum
        
        Tagged_OpDelete (OpDelete charsToDelete_Count) ->
            let
                sum = _charDeleteCountSum model + charsToDelete_Count
                len = length $ _string model
                appendage = unpack $ takeEnd charsToDelete_Count $ pack $ _string model
            in
                if sum > charDeleteCountSum_UpperLimit then
                    Left $ "The total char delete count (for Delete) must be <= " ++ show charDeleteCountSum_UpperLimit ++ ", but instead is " ++ show sum
                else if len == 0 then
                    Left $ "String may not be empty"
                else if charsToDelete_Count == 0 || charsToDelete_Count > len then
                    Left $ "1 <= count <= string length"
                else
                    let
                        model' =
                            Model
                                (unpack $ dropEnd charsToDelete_Count $ pack $ _string model)
                                (_undos model ++ [Model_OpUndoDelete $ OpUndoDelete appendage])
                                (_appendageLengthSum model)
                                sum
                                (_printOutput model)
                    in
                        performOps model' xs

        Tagged_OpPrint (OpPrint pos) ->
                case ((pos - 1) <= (length $ _string model)) of
                    True ->
                        let
                            model' =
                                Model            
                                    (_string model)
                                    (_undos model)
                                    (_appendageLengthSum model)
                                    (_charDeleteCountSum model)
                                    (_printOutput model ++ [_string model !! (pos - 1)] ++ "\n") -- safe
                        in
                            performOps model' xs

                    False ->
                        Left $ "Char position for Print exceeds string length"

        Tagged_OpUndo _ ->
            case null $ _undos model of
                True ->
                    performOps model xs

                False ->
                    let
                        tagged = case last $ _undos model of
                            Model_OpUndoAppend x -> Tagged_OpUndoAppend x
                            Model_OpUndoDelete x -> Tagged_OpUndoDelete x
                        eiModel = performOps model [tagged]
                    in
                        case eiModel of
                            Right model' ->
                                let 
                                    model'' = 
                                        Model
                                            (_string model')
                                            (init $ _undos model')
                                            (_appendageLengthSum model')
                                            (_charDeleteCountSum model')
                                            (_printOutput model')        
                                in
                                    performOps model'' xs
                            Left _ ->
                                eiModel

        Tagged_OpUndoAppend (OpUndoAppend charsToDelete_Count) -> -- todo refactor w/ Tagged_OpDelete
            let
                len = length $ _string model
            in
                if len == 0 then
                    Left $ "String may not be empty"
                else if charsToDelete_Count == 0 || charsToDelete_Count > len then
                    Left $ "1 <= count <= string length"
                else
                    let
                        model' =
                            Model
                                (unpack $ dropEnd charsToDelete_Count $ pack $ _string model)
                                (_undos model)
                                (_appendageLengthSum model)
                                (_charDeleteCountSum model)
                                (_printOutput model)
                    in
                        performOps model' xs

        Tagged_OpUndoDelete (OpUndoDelete appendage) -> -- todo refactor w/ Append above
            let
                len = length appendage
            in
                let
                    model' =
                        Model
                            (_string model ++ appendage)
                            (_undos model)
                            (_appendageLengthSum model)
                            (_charDeleteCountSum model)
                            (_printOutput model)
                in
                    performOps model' xs


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
                        1 ->
                            case tailMay tokens of 
                                Just args ->                                    
                                    case length args == 1 of
                                        True -> do
                                            Right $ Tagged_OpAppend $ OpAppend $ head args
                                        False ->
                                            Left $ errorWithLineNum n "Append has one arg"  
                                Nothing ->
                                    Left $ errorWithLineNum n "Append has one arg"   
                        2 ->
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
                        3 ->
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
                        4 ->
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
