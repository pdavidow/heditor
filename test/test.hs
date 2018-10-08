import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import System.Directory (doesFileExist , removeFile)
import System.IO
import Data.List (foldl1')
import Control.Monad (when)

import Scanner (scanFile, opCount_UpperLimit, appendageLengthSum_UpperLimit, charDeleteCountSum_UpperLimit)
import TestHelper ( genFile_exceed__appendageLengthSum_UpperLimit, genFile_exceed__charDeleteCountSum_UpperLimit)
  
main = defaultMain tests
 
tests :: TestTree
tests = testGroup "Tests" [unitTests] 

unitTests = testGroup "Unit tests" $
    [ testGroup "scan existing" $
        [ testCase "good 1" $ do
            let input = "./test/input/good1.txt" 
            let output = "./test/temp_good1.txt"
            scanFile input output 
            result <- readFile output
            result @?= "c\ny\na\n"
            removeFile output

        , testCase "good 2" $ do
            let input = "./test/input/good2.txt" 
            let output = "./test/temp_good2.txt"
            scanFile input output 
            result <- readFile output
            result @?= "c\ny\na\n"
            removeFile output           
 
        , testCase "good 3" $ do
            let input = "./test/input/good3.txt" 
            let output = "./test/temp_good3.txt"
            scanFile input output 
            result <- readFile output
            result @?= "c\ny\na\n"
            removeFile output
 
        , testCase "bad 1" $ do
            let input = "./test/input/bad1.txt" 
            let output = "./test/temp_bad1.txt"
            result <- scanFile input output
            result @?= (Left "Operation count does not match actual, line 1")
            isOkToRemove <- doesFileExist output
            when isOkToRemove $ removeFile output    

 
        , testCase "bad 2" $ do
            let input = "./test/input/bad2.txt" 
            let output = "./test/temp_bad2.txt"
            result <- scanFile input output
            result @?= (Left "Operation count expected, line 1")
            isOkToRemove <- doesFileExist output
            when isOkToRemove $ removeFile output               
  
 
        , testCase "bad 3" $ do
            let input = "./test/input/bad3.txt" 
            let output = "./test/temp_bad3.txt"
            result <- scanFile input output
            result @?= (Left "Operation count does not match actual, line 1")
            isOkToRemove <- doesFileExist output
            when isOkToRemove $ removeFile output  
 
        , testCase "bad 4" $ do
            let input = "./test/input/bad4.txt" 
            let output = "./test/temp_bad4.txt"
            result <- scanFile input output
            result @?= (Left "Operation count expected, line 1")
            isOkToRemove <- doesFileExist output
            when isOkToRemove $ removeFile output  
 
        , testCase "bad 5" $ do
            let input = "./test/input/bad5.txt" 
            let output = "./test/temp_bad5.txt"
            result <- scanFile input output
            result @?= (Left "Operation count expected, line 1")
            isOkToRemove <- doesFileExist output
            when isOkToRemove $ removeFile output 

        , testCase "bad 6" $ do
            let input = "./test/input/bad6.txt" 
            let output = "./test/temp_bad6.txt"
            result <- scanFile input output
            result @?= (Left "Operation count expected, line 1")
            isOkToRemove <- doesFileExist output
            when isOkToRemove $ removeFile output 
   
        , testCase "bad 7" $ do
            let input = "./test/input/bad7.txt" 
            let output = "./test/temp_bad7.txt"
            result <- scanFile input output
            result @?= (Left "Invalid operation type, line 2")
            isOkToRemove <- doesFileExist output
            when isOkToRemove $ removeFile output 
   
        , testCase "bad 8a" $ do
            let input = "./test/input/bad8a.txt" 
            let output = "./test/temp_bad8a.txt"
            result <- scanFile input output
            result @?= (Left "All input characters are lowercase English letters, line 2")
            isOkToRemove <- doesFileExist output
            when isOkToRemove $ removeFile output 
   
        , testCase "bad 8b" $ do
            let input = "./test/input/bad8b.txt" 
            let output = "./test/temp_bad8b.txt"
            result <- scanFile input output
            result @?= (Left "All input characters are lowercase English letters, line 2")
            isOkToRemove <- doesFileExist output
            when isOkToRemove $ removeFile output    

        , testCase "bad 9" $ do
            let input = "./test/input/bad9.txt" 
            let output = "./test/temp_bad9.txt"
            result <- scanFile input output
            result @?= (Left "1 <= count <= string length, line 3")
            isOkToRemove <- doesFileExist output
            when isOkToRemove $ removeFile output   

        , testCase "bad 10" $ do
            let input = "./test/input/bad10.txt" 
            let output = "./test/temp_bad10.txt"
            result <- scanFile input output
            result @?= (Left "Char position for Print exceeds string length, line 4")
            isOkToRemove <- doesFileExist output
            when isOkToRemove $ removeFile output  

        , testCase "bad 11" $ do
            let input = "./test/input/bad11.txt" 
            let output = "./test/temp_bad11.txt"
            result <- scanFile input output
            result @?= (Left "Operation count must be a positive integer <= 1000000, line 1")
            isOkToRemove <- doesFileExist output
            when isOkToRemove $ removeFile output 
 
        , testCase "bad 12" $ do
            let input = "./test/input/bad12.txt" 
            let output = "./test/temp_bad12.txt"
            result <- scanFile input output
            result @?= (Left "Char position for Print exceeds string length, line 14")
            isOkToRemove <- doesFileExist output
            when isOkToRemove $ removeFile output                 
        ]

    , testGroup "scan generated" $
        [ testCase "good appendageLengthSum_UpperLimit" $ do
            let filePath = "good_Gen1.txt"
            let excess = 0
            genFile_exceed__appendageLengthSum_UpperLimit filePath excess

            let input = filePath
            let output = "./test/temp_goodGen1.txt"
            result <- scanFile input output 
            result @?= (Right $ ())
            removeFile input
            isOkToRemove <- doesFileExist output
            when isOkToRemove $ removeFile output  

        , testCase "bad appendageLengthSum_UpperLimit" $ do
            let filePath = "bad_Gen1.txt"
            let excess = 1
            genFile_exceed__appendageLengthSum_UpperLimit filePath excess

            let input = filePath
            let output = "./test/temp_goodGen1.txt"
            result <- scanFile input output 
            result @?= (Left $ "Sum of lengths for all appendage args (optype 1) must be <= 1000000 (actual 1000001), line 202")
            removeFile input
            isOkToRemove <- doesFileExist output
            when isOkToRemove $ removeFile output  

        , testCase "good charDeleteCountSum_UpperLimit" $ do
            let filePath = "good_Gen2.txt"
            let excess = 0
            genFile_exceed__charDeleteCountSum_UpperLimit filePath excess

            let input = filePath
            let output = "./test/temp_goodGen1.txt"
            result <- scanFile input output 
            result @?= (Right $ ())
            removeFile input
            isOkToRemove <- doesFileExist output
            when isOkToRemove $ removeFile output  
  
        , testCase "bad charDeleteCountSum_UpperLimit" $ do
            let filePath = "bad_Gen2.txt"
            let excess = 1
            genFile_exceed__charDeleteCountSum_UpperLimit filePath excess

            let input = filePath
            let output = "./test/temp_goodGen1.txt"
            result <- scanFile input output 
            result @?= (Left $ "The total char delete count (for Delete) must be <= 2000000, but instead is 2000001, line 802")
            removeFile input
            isOkToRemove <- doesFileExist output
            when isOkToRemove $ removeFile output          
        ]
    ]