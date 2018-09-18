import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import System.Directory (removeFile)
import System.IO
import Data.List (foldl1')

import Scanner (scanFile, opCount_UpperLimit, appendageLengthSum_UpperLimit, charDeleteCountSum_UpperLimit)
import TestHelper ( genFile_exceed__appendageLengthSum_UpperLimit, genFile_exceed__charDeleteCountSum_UpperLimit)
  
main = defaultMain tests
 
tests :: TestTree
tests = testGroup "Tests" [unitTests] 

unitTests = testGroup "Unit tests" $
    [ testGroup "scan existing" $
        [ testCase "good 1" $ do
            result <- scanFile "./test/input/test_input_good_1.txt"
            result @?= (Right $ "c\ny\na\n")
            pure ()

        , testCase "good 2" $ do
            result <- scanFile "./test/input/test_input_good_2.txt"
            result @?= (Right $ "c\ny\na\n")
            pure ()   

        , testCase "good 3" $ do
            result <- scanFile "./test/input/test_input_good_3.txt"
            result @?= (Right $ "c\ny\na\n")
            pure ()   

        , testCase "bad 1" $ do
            result <- scanFile "./test/input/test_input_bad_1.txt"
            result @?= (Left "Operation count does not match actual, line 1")
            pure ()   

        , testCase "bad 2" $ do
            result <- scanFile "./test/input/test_input_bad_2.txt"
            result @?= (Left "Operation count expected, line 1")
            pure ()      

        , testCase "bad 3" $ do
            result <- scanFile "./test/input/test_input_bad_3.txt"
            result @?= (Left "Operation count does not match actual, line 1")
            pure ()   

        , testCase "bad 4" $ do
            result <- scanFile "./test/input/test_input_bad_4.txt"
            result @?= (Left "Operation count expected, line 1")
            pure ()    

        , testCase "bad 5" $ do
            result <- scanFile "./test/input/test_input_bad_5.txt"
            result @?= (Left "Operation count expected, line 1")
            pure ()    

        , testCase "bad 6" $ do
            result <- scanFile "./test/input/test_input_bad_6.txt"
            result @?= (Left "Operation count expected, line 1")
            pure ()    

        , testCase "bad 7" $ do
            result <- scanFile "./test/input/test_input_bad_7.txt"
            result @?= (Left "Invalid operation type, line 2")
            pure ()    

        , testCase "bad 8a" $ do
            result <- scanFile "./test/input/test_input_bad_8a.txt"
            result @?= (Left "All input characters are lowercase English letters, line 2")
            pure ()   

        , testCase "bad 8b" $ do
            result <- scanFile "./test/input/test_input_bad_8b.txt"
            result @?= (Left "All input characters are lowercase English letters, line 2")
            pure ()     

        , testCase "bad 9" $ do
            result <- scanFile "./test/input/test_input_bad_9.txt"
            result @?= (Left "1 <= count <= string length, line 3")
            pure ()   

        , testCase "bad 10" $ do
            result <- scanFile "./test/input/test_input_bad_10.txt"
            result @?= (Left "Char position for Print exceeds string length, line 4")
            pure ()   

        , testCase "bad 11" $ do
            result <- scanFile "./test/input/test_input_bad_11.txt"
            result @?= (Left "Operation count must be a positive integer <= 1000000, line 1")
            pure ()  

        , testCase "bad 12" $ do
            result <- scanFile "./test/input/test_input_bad_12.txt"
            result @?= (Left "Char position for Print exceeds string length, line 14")
            pure ()                 
        ]  

    , testGroup "scan generated" $
        [ testCase "good appendageLengthSum_UpperLimit" $ do
            let filePath = "good_Gen1.txt"
            let excess = 0
            genFile_exceed__appendageLengthSum_UpperLimit filePath excess
            result <- scanFile filePath
            result @?= (Right $ "")
            removeFile filePath
            pure ()     

        , testCase "bad appendageLengthSum_UpperLimit" $ do
            let filePath = "bad_Gen1.txt"
            let excess = 1
            genFile_exceed__appendageLengthSum_UpperLimit filePath excess
            result <- scanFile filePath
            result @?= (Left $ "Sum of lengths for all appendage args (optype 1) must be <= 1000000 (actual 1000001), line 202")
            removeFile filePath
            pure ()  

        , testCase "good charDeleteCountSum_UpperLimit" $ do
            let filePath = "good_Gen2.txt"
            let excess = 0
            genFile_exceed__charDeleteCountSum_UpperLimit filePath excess
            result <- scanFile filePath
            result @?= (Right $ "")
            removeFile filePath
            pure ()   
            
        , testCase "bad charDeleteCountSum_UpperLimit" $ do
            let filePath = "bad_Gen2.txt"
            let excess = 1
            genFile_exceed__charDeleteCountSum_UpperLimit filePath excess
            result <- scanFile filePath
            result @?= (Left $ "The total char delete count (for Delete) must be <= 2000000, but instead is 2000001, line 802")
            removeFile filePath
            pure ()               
        ]
    ]