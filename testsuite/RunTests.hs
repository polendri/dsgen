module RunTests where

import Test.HUnit(runTestTT)

import qualified Dsgen.TestCards as TestCards
import qualified Dsgen.TestSetSelect as TestSetSelect

main = do
    runTestTT TestCards.hunitTests
    runTestTT TestSetSelect.hunitTests

