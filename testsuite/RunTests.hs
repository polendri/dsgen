module RunTests where

import Test.HUnit(runTestTT)

import qualified Dsgen.TestCards as TestCards
import qualified Dsgen.TestSetSelect as TestSetSelect

hunitTests = [
    TestCards.hunitTests,
    TestSetSelect.hunitTests
    ]

quickCheckTests = [
    TestSetSelect.quickCheckTests
    ]


runTests hs qcs = do
    putStr "\n"
    putStr "Running HUnit tests...\n\n"
    mapM_ runTestTT hs
    putStr "\n\n"
    putStr "Running QuickCheck tests...\n\n"
    mapM_ id qcs
    putStr "\n"

{-
runQCTests :: IO Bool -> IO ()
runQCTests rt = rt >>= \passed -> if passed
                                  then putStrLn $ "All tests passed."
                                  else putStrLn $ "Some tests failed."
-}

main = runTests hunitTests quickCheckTests

