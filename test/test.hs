import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Scanner (scanFile)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests] 

unitTests = testGroup "Unit tests" $
    [ testGroup "Scanner" $
        [ testCase "scanFile" $ do
            result <- scanFile "test/input/temp1.txt"
            result @?= ["30"] 
            pure ()
        ]
    ]