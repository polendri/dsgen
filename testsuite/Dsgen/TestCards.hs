module Dsgen.TestCards where

import Control.Monad.Error
import Data.ConfigFile
import Test.HUnit

import Dsgen.Cards


{- Test definitions -}

amountReadTest = TestCase $ do
    assertEqual "\"Variable\" read failed"
                Variable
                (read "Variable" :: Amount)
    assertEqual "\"FixedAmount 1\" read failed"
                (FixedAmount 1)
                (read "1" :: Amount)

readCardTest = TestCase $ do
    cpE <- runErrorT $ readstring emptyCP sampleCardConfigString
    either (\x -> error "ConfigFile parsing failed")
           (\cp -> do
               card <- runErrorT $ readCard (cp { optionxform = id}) "Test"
               assertEqual "Card read failed"
                           (Right sampleCard)
                           card)
           cpE


{- Test list -}

hunitTests = TestList [
    TestLabel "Amount read test" amountReadTest,
    TestLabel "readCard test" readCardTest
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

sampleCardConfigString = unlines [
    "[Test]",
    "source = Dominion",
    "cost = 1",
    "potionCost = 0",
    "categories = [Action, Attack]",
    "plusCards = 0",
    "plusActions = Variable",
    "plusBuys = 1",
    "plusCoins = 6",
    "blocksAttacks = False",
    "givesCurses = True",
    "gainsCards = True",
    "trashesCards = False",
    "trashesItself = False",
    "interactive = True",
    "complexity = High"
    ]

