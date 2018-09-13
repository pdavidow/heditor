import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Scanner (scanFile)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests] 

unitTests = testGroup "Unit tests" $
    [ testGroup "scanFile" $
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
        , testCase "bad 8" $ do
            result <- scanFile "./test/input/test_input_bad_8.txt"
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
            result @?= (Left "Operation count does not match actual, line 1")
            pure ()  
        , testCase "bad 12" $ do
            result <- scanFile "./test/input/test_input_bad_12.txt"
            result @?= (Left "Char position for Print exceeds string length, line 14")
            pure ()                                                                                                                  
        ]                  
    ]