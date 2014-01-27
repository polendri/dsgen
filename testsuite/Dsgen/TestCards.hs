{-# LANGUAGE TemplateHaskell #-}

module Dsgen.TestCards where

import Control.Monad(liftM)
import Control.Monad.Error
import Data.ConfigFile
import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.All

import Dsgen.Cards
import Dsgen.TestHelpers


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

quickCheckTests = $quickCheckAll


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


{- QuickCheck definitions for types -}

instance Arbitrary Card where
    arbitrary = do
        name          <- cardNameGen
        source        <- arbitrary
        cost          <- arbitrary
        potionCost    <- arbitrary
        categories    <- subset [Action, Attack, Reaction, Duration,
                              Treasure, Victory, Looter]
        plusCards     <- arbitrary
        plusActions   <- arbitrary
        plusBuys      <- arbitrary
        plusCoins     <- arbitrary
        blocksAttacks <- arbitrary
        givesCurses   <- arbitrary
        gainsCards    <- arbitrary
        trashesCards  <- arbitrary
        trashesItself <- arbitrary
        interactive   <- arbitrary
        complexity    <- arbitrary
        return Card {
            cardName          = name,
            cardSource        = source,
            cardCost          = cost,
            cardPotionCost    = potionCost,
            cardCategories    = categories,
            cardPlusCards     = plusCards,
            cardPlusActions   = plusActions,
            cardPlusBuys      = plusBuys,
            cardPlusCoins     = plusCoins,
            cardBlocksAttacks = blocksAttacks,
            cardGivesCurses   = givesCurses,
            cardGainsCards    = gainsCards,
            cardTrashesCards  = trashesCards,
            cardTrashesItself = trashesItself,
            cardInteractive   = interactive,
            cardComplexity    = complexity
            }

instance Arbitrary CardSource where
    arbitrary = elements [Dominion, Intrigue, Seaside, Alchemy,
                          Prosperity, Cornucopia, Hinterlands, DarkAges,
                          Guilds, EnvoyPromo, BlackMarketPromo, StashPromo,
                          WalledVillagePromo, GovernorPromo, Custom]

instance Arbitrary Amount where
    arbitrary = do
        n <- choose (-1, 10) :: Gen Int
        return $ if n == -1 then Variable else FixedAmount n

instance Arbitrary CardCategory where
    arbitrary = elements [Action, Attack, Reaction, Duration,
                          Treasure, Victory, Looter]

instance Arbitrary CardComplexity where
    arbitrary = elements [Low, Medium, High]


{- Helper methods -}

cardNameGen :: Gen String
cardNameGen = frequency [(1,   return "Moat"),
                         (204, listOf1 $ elements (['A'..'Z'] ++ ['a'..'z']))]

