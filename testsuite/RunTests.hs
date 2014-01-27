-- | Driver module for running the test suite.
module RunTests where

import Test.HUnit(runTestTT, errors, failures)

import qualified Dsgen.TestCards as TestCards
import qualified Dsgen.TestSetSelect as TestSetSelect


{- Test collection lists -}

hunitTests = [
    TestCards.hunitTests,
    TestSetSelect.hunitTests
    ]

quickCheckTests = [
    TestSetSelect.quickCheckTests
    ]


{- Test runner functions -}

runHUnitTests hs = do
    counts <- runTestTT hs
    return ()

runQuickCheckTests qcs = do
    success <- qcs
    return ()

runTests hs qcs = do
    putStr "\n"
    putStr "Running HUnit tests...\n\n"
    mapM_ runHUnitTests hs
    putStr "\n\n"
    putStr "Running QuickCheck tests...\n\n"
    mapM_ runQuickCheckTests qcs
    putStr "\n"

main = runTests hunitTests quickCheckTests

