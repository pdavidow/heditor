module TestHelper
    ( genFile_exceed__appendageLengthSum_UpperLimit
    , genFile_exceed__charDeleteCountSum_UpperLimit
    )
    where
 
import System.Directory (removeFile)
import System.IO

import Scanner (scanFile, opCount_UpperLimit, appendageLengthSum_UpperLimit, charDeleteCountSum_UpperLimit)


genFile_exceed__appendageLengthSum_UpperLimit :: FilePath -> Int -> IO ()
genFile_exceed__appendageLengthSum_UpperLimit filePath excess = do
    let totalLength = appendageLengthSum_UpperLimit + excess
    let sectionLength = 5000
    let appendages = monoCharStrings totalLength sectionLength

    let opCount_Append = length appendages 
    let opCount_Delete = opCount_Append
    let opCount_Undo = opCount_Append
    let opCount = opCount_Append * 3

    handle <- openFile filePath WriteMode
    write_OpCount handle opCount
    write_AppendOps handle appendages
    write_DeleteOps handle opCount_Delete sectionLength
    write_UndoOps handle opCount_Undo
    hClose handle


genFile_exceed__charDeleteCountSum_UpperLimit :: FilePath -> Int -> IO ()
genFile_exceed__charDeleteCountSum_UpperLimit filePath excess = do
    let totalLength = appendageLengthSum_UpperLimit
    let sectionLength = 5000
    let appendages = monoCharStrings totalLength sectionLength
    let isExceedingLimit = excess > 0

    let deleteOp_CharCount1 = sectionLength
    let deleteOp_CharCount2 = deleteOp_CharCount1
    let deleteOp_CharCount3 = excess

    let opCount_Append = length appendages 
    --traceIO $ "opCount_Append: " ++ show opCount_Append
    let opCount_Delete1 = opCount_Append
    let opCount_Undo = opCount_Delete1
    let opCount_Delete2 = opCount_Delete1
    let opCount_Delete3 = if isExceedingLimit then 1 else 0
    let opCount = opCount_Append + opCount_Delete1 + opCount_Undo + opCount_Delete2 + opCount_Delete3

    handle <- openFile filePath WriteMode
    write_OpCount handle opCount
    write_AppendOps handle appendages
    write_DeleteOps handle opCount_Delete1 deleteOp_CharCount1
    write_UndoOps handle opCount_Undo
    write_DeleteOps handle opCount_Delete2 deleteOp_CharCount2
    if isExceedingLimit then do
        write_DeleteOps handle opCount_Delete3 deleteOp_CharCount3
    else
        pure ()
    hClose handle


monoCharStrings :: Int -> Int -> [String]
monoCharStrings totalLength sectionLength =
    -- Assume sectionLength <= limit
    makeMonoCharStrings totalLength sectionLength 0 []


makeMonoCharStrings :: Int -> Int -> Int -> [String] -> [String]
makeMonoCharStrings totalLength sectionLength currentSumLength acc =
    if currentSumLength >= totalLength then
        acc
    else
        let
            remainingLength = totalLength - currentSumLength
            charCount = if remainingLength >= sectionLength then sectionLength else remainingLength
            string = monoCharString charCount
        in
            makeMonoCharStrings totalLength sectionLength (currentSumLength + length string) $ acc ++ [string]


write_OpCount :: Handle -> Int -> IO ()    
write_OpCount handle n =
    hPutStrLn handle $ show n


write_AppendOps :: Handle -> [String] -> IO ()
write_AppendOps handle xs = do
    let ops = concatMap (\ x -> "1 " ++ x ++ "\n") xs
    hPutStr handle ops


write_DeleteOps :: Handle -> Int -> Int -> IO ()
write_DeleteOps handle opCount_Delete sectionLength = do
    let line = "2 " ++ show sectionLength ++ "\n"
    let ops = concatMap (\ _ -> line) [1 .. opCount_Delete]
    hPutStr handle ops


write_UndoOps :: Handle -> Int -> IO ()
write_UndoOps handle opCount_Undo = do
    let ops = concatMap (\ _ -> "4\n") $ [1 .. opCount_Undo]
    hPutStr handle ops


monoCharString :: Int -> String
monoCharString len =
    replicate len 'c' -- arbitrary        