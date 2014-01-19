module Dsgen.TestSetSelect where

import Test.HUnit

import Dsgen.Cards
import Dsgen.SetSelect


{- Test definitions -}

actionFilterTests = TestCase $ do
    assertEqual "actionFilter failed to filter Victory"
                False
                (actionFilter $ sampleCard { cardCategories = [Action, Victory] })
    assertEqual "actionFilter failed to filter Treasure"
                False
                (actionFilter $ sampleCard { cardCategories = [Action, Treasure] })
    assertEqual "actionFilter filtered unexpectedly"
                True
                (actionFilter $ sampleCard { cardCategories = [Action, Attack, Reaction, Duration, Looter] })

amountReadTest = TestCase $ do
    assertEqual "\"Variable\" read failed"
                Variable
                (read "Variable" :: Amount)
    assertEqual "\"FixedAmount 1\" read failed"
                (FixedAmount 1)
                (read "1" :: Amount)


{- Test list -}

hunitTests = TestList [
    TestLabel "actionFilter tests" actionFilterTests
    ]


{- Sample data -}

sampleCard = Card {
    cardName = "Test",
    cardSource = Dominion,
    cardCost = FixedAmount 1,
    cardPotionCost = FixedAmount 0,
    cardCategories = [Action, Attack],
    cardPlusCards = FixedAmount 0,
    cardPlusActions = Variable,
    cardPlusBuys = FixedAmount 1,
    cardPlusCoins = FixedAmount 6,
    cardBlocksAttacks = False,
    cardGivesCurses = True,
    cardGainsCards = True,
    cardTrashesCards = False,
    cardTrashesItself = False,
    cardInteractive = True,
    cardComplexity = High
    }

