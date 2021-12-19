module Scanner
    ( scanFile
    , opCount_UpperLimit       
    , appendageLengthSum_UpperLimit
    , charDeleteCountSum_UpperLimit
    )
    where
        
import           Control.Monad.IO.Class (MonadIO)        
import           Control.Monad.Trans.Except (ExceptT(..), except , runExceptT)        
import           Data.Char (isAlpha, isLower)
import           Data.String.Utils (rstrip)
import           Data.Text (dropEnd, pack, takeEnd, unpack)
import           Safe (atMay, headMay, tailMay) 
import           Text.Read (readMaybe)


data OpAppend = OpAppend String LineNum deriving (Eq, Show)
data OpDelete = OpDelete Int LineNum deriving (Eq, Show)
data OpPrint = OpPrint Int LineNum deriving (Eq, Show)
data OpUndo = OpUndo LineNum deriving (Eq, Show)
data OpUndoAppend = OpUndoAppend Int LineNum deriving (Eq, Show)
data OpUndoDelete = OpUndoDelete String LineNum deriving (Eq, Show)

data Tagged_Op 
    = Tagged_OpAppend OpAppend 
    | Tagged_OpDelete OpDelete 
    | Tagged_OpPrint OpPrint 
    | Tagged_OpUndo OpUndo
    | Tagged_OpUndoAppend OpUndoAppend
    | Tagged_OpUndoDelete OpUndoDelete

data Tagged_OpUndo
    = Tagged_OpUndo_OpUndoAppend OpUndoAppend
    | Tagged_OpUndo_OpUndoDelete OpUndoDelete

type ErrorMsg = String 
type Line = String      
type LineNum = Int

data Model = Model
    { _string :: String
    , _undos :: [Tagged_OpUndo]
    , _appendageLengthSum :: Int
    , _charDeleteCountSum :: Int
    , _printOutput :: FilePath
    }

    
initialModel :: FilePath -> Model
initialModel output = 
    Model "" [] 0 0 output


opCount_UpperLimit            = 1000000
appendageLengthSum_UpperLimit = 1000000
charDeleteCountSum_UpperLimit = 2000000


scanFile :: MonadIO m => FilePath -> FilePath -> m (Either ErrorMsg ())
scanFile input output = runExceptT $ do
    lines <- getLines input
    case headMay lines of
        Just h -> do
            let mbOpCount = readMaybe h :: Maybe Int
            case mbOpCount of
                Just opCount ->
                    case tailMay lines of
                        Just opLines -> do
                            eiModel <- parseOps output opCount opLines
                            case eiModel of
                                Right model -> do
                                    except $ Right ()

                                Left err ->
                                    except $ Left err

                        Nothing -> do
                            except $ Left $ errorWithLineNum 2 "Operation expected" 

                Nothing -> 
                    except $ Left $ errorWithLineNum 1 "Operation count expected" 

        Nothing -> do
            except $ Left $ errorWithLineNum 1 "Operation count expected" 
    

getLines :: MonadIO m => FilePath -> m [Line]
getLines x = do
    contents <- readFile x -- lazy
    pure $ lines $ rstrip contents    


parseOps :: MonadIO m => FilePath -> Int -> [Line] -> m (Either ErrorMsg Model)
parseOps output opCount xs = 
    if opCount > opCount_UpperLimit then
        pure $ Left $ errorWithLineNum 1 $ "Operation count must be a positive integer <= " ++ show opCount_UpperLimit
    else if opCount /= length xs then
        pure $ Left $ errorWithLineNum 1 "Operation count does not match actual"
    else
        let 
            numberedLines = zip [(2 :: Int)..] xs 
            eiOps = parseCleanOps [] numberedLines
        in
            case eiOps of
                Right ops ->
                    performOps (initialModel output) ops

                Left err -> do
                    pure $ Left err      


parseCleanOps :: [Tagged_Op] -> [(Int, Line)] -> Either ErrorMsg [Tagged_Op]
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


-- todo refactor
parseOp :: (Int, Line) -> Either ErrorMsg Tagged_Op
parseOp (lineNum, line) = do
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
                                            let appendage = head args -- safe
                                            case length appendage == (length $ filter (\x -> isAlpha x && isLower x) appendage) of
                                                True -> 
                                                    Right $ Tagged_OpAppend $ OpAppend appendage lineNum
                                                False -> 
                                                    Left $ errorWithLineNum lineNum "All input characters are lowercase English letters"  
                                        False ->
                                            Left $ errorWithLineNum lineNum "Append has one arg"  
                                Nothing ->
                                    Left $ errorWithLineNum lineNum "Append has one arg"   
                        2 ->
                            case tailMay tokens of
                                Just args ->
                                    case length args == 1 of
                                        True -> do
                                            let mbArg = readMaybe $ head args :: Maybe Int -- safe head
                                            case mbArg of
                                                Just arg -> 
                                                    Right $ Tagged_OpDelete $ OpDelete arg lineNum
                                                Nothing -> 
                                                    Left $ errorWithLineNum lineNum "Delete has one int arg"
                                        False ->
                                            Left $ errorWithLineNum lineNum "Delete has one arg"  
                                Nothing ->
                                    Left $ errorWithLineNum lineNum "Delete has one arg"                             
                        3 ->
                            case tailMay tokens of
                                Just args ->
                                    case length args == 1 of
                                        True -> do
                                            let mbArg = readMaybe $ head args :: Maybe Int -- safe head
                                            case mbArg of
                                                Just arg -> 
                                                    Right $ Tagged_OpPrint $ OpPrint arg lineNum
                                                Nothing -> 
                                                    Left $ errorWithLineNum lineNum "Print has one int arg"
                                        False ->
                                            Left $ errorWithLineNum lineNum "Print has one arg"  
                                Nothing ->
                                    Left $ errorWithLineNum lineNum "Print has one arg"                               
                        4 ->
                            case tokenLength == 1 of
                                True ->
                                    Right $ Tagged_OpUndo $ OpUndo lineNum
                                False ->
                                    Left $ errorWithLineNum lineNum "Undo has no args"    
                        _ -> 
                            Left $ errorWithLineNum lineNum "Invalid operation type"     
                Nothing ->
                    Left $ errorWithLineNum lineNum "Invalid operation type"     
        False ->
            Left $ errorWithLineNum lineNum "Operation type expected"       
    

performOps :: MonadIO m => Model -> [Tagged_Op] -> m (Either ErrorMsg Model)
performOps model [] =
    pure $ Right model

performOps model (op : xs) =
    case op of
        Tagged_OpAppend (OpAppend appendage lineNum) ->
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
                                    (_undos model ++ [Tagged_OpUndo_OpUndoAppend $ OpUndoAppend len 0]) -- dummy lineNum
                                    sum
                                    (_charDeleteCountSum model)
                                    (_printOutput model)
                        in
                            performOps model' xs

                    False ->
                        pure $ Left $ errorWithLineNum lineNum $ "Sum of lengths for all appendage args (optype 1) must be <= " ++ show appendageLengthSum_UpperLimit ++ " (actual " ++ show sum ++ ")"
        
        Tagged_OpDelete (OpDelete charsToDelete_Count lineNum) ->
            let
                sum = _charDeleteCountSum model + charsToDelete_Count
                appendage = unpack $ takeEnd charsToDelete_Count $ pack $ _string model
            in
                if sum > charDeleteCountSum_UpperLimit then
                    pure $ Left $ errorWithLineNum lineNum $ "The total char delete count (for Delete) must be <= " ++ show charDeleteCountSum_UpperLimit ++ ", but instead is " ++ show sum
                else 
                    case basicDelete charsToDelete_Count lineNum model of
                        Right model' -> 
                            let                                
                                model'' =
                                    Model
                                        (_string model')
                                        (_undos model' ++ [Tagged_OpUndo_OpUndoDelete $ OpUndoDelete appendage 0]) -- dummy lineNum
                                        (_appendageLengthSum model')
                                        sum
                                        (_printOutput model')
                            in
                                performOps model'' xs

                        Left err -> 
                            pure $ Left err

        Tagged_OpPrint (OpPrint pos lineNum) ->
                case (pos - 1) < (length $ _string model) of
                    True -> do
                        appendFile (_printOutput model) $ [_string model !! (pos - 1)] ++ "\n" -- safe

                        let model' = Model            
                                (_string model)
                                (_undos model)
                                (_appendageLengthSum model)
                                (_charDeleteCountSum model)
                                (_printOutput model) 

                        performOps model' xs

                    False ->
                        pure $ Left $ errorWithLineNum lineNum $ "Char position for Print exceeds string length"

        Tagged_OpUndo (OpUndo lineNum) ->
            case null $ _undos model of
                True ->
                    performOps model xs

                False -> do
                    let taggedOp = case last $ _undos model of
                            Tagged_OpUndo_OpUndoAppend (OpUndoAppend x _) -> Tagged_OpUndoAppend (OpUndoAppend x lineNum)
                            Tagged_OpUndo_OpUndoDelete (OpUndoDelete x _) -> Tagged_OpUndoDelete (OpUndoDelete x lineNum)
                    
                    result <- performOps model [taggedOp]

                    case result of
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

                        Left err -> 
                            pure $ Left err

        Tagged_OpUndoAppend (OpUndoAppend charsToDelete_Count lineNum) ->
            case basicDelete charsToDelete_Count lineNum model of
                Right model' -> 
                    performOps model' xs

                Left err -> 
                    pure $ Left err

        Tagged_OpUndoDelete (OpUndoDelete appendage lineNum) -> 
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


basicDelete :: Int -> LineNum -> Model -> Either ErrorMsg Model
basicDelete charsToDelete_Count lineNum model =
    let
        len = length $ _string model
    in
        if len == 0 then
            Left $ errorWithLineNum lineNum $ "String may not be empty"
        else if charsToDelete_Count == 0 || charsToDelete_Count > len then
            Left $ errorWithLineNum lineNum $ "1 <= count <= string length"
        else
            Right $ 
                Model
                    (unpack $ dropEnd charsToDelete_Count $ pack $ _string model)
                    (_undos model)
                    (_appendageLengthSum model)
                    (_charDeleteCountSum model)
                    (_printOutput model)  


errorWithLineNum :: Int -> String -> String
errorWithLineNum lineNum error =
    error ++ ", line " ++ show lineNum
