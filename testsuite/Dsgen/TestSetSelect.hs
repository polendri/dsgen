{-# LANGUAGE TemplateHaskell #-}

module Dsgen.TestSetSelect where

import Control.Monad(liftM)
import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.All

import Dsgen.Cards
import Dsgen.SetSelect


{- Helper methods -}

elemAny :: Eq a => [a] -> [a] -> Bool
elemAny [] _ = False
elemAny (e:es) l = if elem e l then True else elemAny es l

notElemAny :: Eq a => [a] -> [a] -> Bool
notElemAny es l = not $ elemAny es l

-- | Picks a random subset from the provided list
subset :: [a] -> Gen [a]
subset [] = return []
subset (x:xs) = do
    pickX <- arbitrary :: Gen Bool
    if pickX then liftM (x:) rest else rest
  where rest = subset xs

cardNameGen :: Gen String
cardNameGen = frequency [(1,   return "Moat"),
                         (204, listOf1 $ elements (['A'..'Z'] ++ ['a'..'z']))]


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


{- Test definitions -}

prop_actionFilterPositive c = notElemAny [Treasure, Victory] (cardCategories c) ==> actionFilter c == False

prop_actionFilterNegative c = notElemAny [Treasure, Victory] (cardCategories c) ==> actionFilter c == True

{-
complexityFilterTests = TestCase $ do
    assertEqual "complexityFilter failed to filter"
                False
                (complexityFilter MediumComplexityFilterOption $ sampleCard { cardComplexity = High })
    assertEqual "complexityFilter filtered unexpectedly"
                True
                (complexityFilter MediumComplexityFilterOption $ sampleCard { cardComplexity = Medium })

costVarietyRuleTests = TestCase $ do
    assertEqual "cardVarietyRule failed to return False"
                False
                (costVarietyRule $ [sampleCard { cardCost = FixedAmount 2 },
                                    sampleCard { cardCost = FixedAmount 4 },
                                    sampleCard { cardCost = FixedAmount 4 },
                                    sampleCard { cardCost = FixedAmount 5 }])
    assertEqual "cardVarietyRule failed to return True"
                True
                (costVarietyRule $ [sampleCard { cardCost = FixedAmount 2 },
                                    sampleCard { cardCost = FixedAmount 3 },
                                    sampleCard { cardCost = FixedAmount 4 },
                                    sampleCard { cardCost = FixedAmount 5 }])

interactivityRuleTests = TestCase $ do
    assertEqual "interactivityRule negative failed"
                False
                (interactivityRule 2 $ [sampleCard { cardInteractive = True }])
    assertEqual "interactivityRule positive failed"
                True
                (interactivityRule 1 $ [sampleCard { cardInteractive = True }])

reactionRuleTests = TestCase $ do
    assertEqual "reactionRule Moat negative failed"
                False
                (reactionRule MoatReaction $ [
                     sampleCard { cardCategories = [Action, Attack] },
                     sampleCard { cardCategories = [Victory] }
                     ])
    assertEqual "reactionRule Moat positive failed"
                True
                (reactionRule MoatReaction $ [
                     sampleCard { cardCategories = [Action, Attack] },
                     sampleCard { cardName = "Moat" }
                     ])
    assertEqual "reactionRule blocker negative failed"
                False
                (reactionRule BlockerReaction $ [
                     sampleCard { cardCategories = [Action, Attack] },
                     sampleCard { cardCategories = [Victory] }
                     ])
    assertEqual "reactionRule blocker positive failed"
                True
                (reactionRule BlockerReaction $ [
                     sampleCard { cardCategories = [Action, Attack] },
                     sampleCard { cardBlocksAttacks = True }
                     ])
    assertEqual "reactionRule reaction negative failed"
                False
                (reactionRule ReactionReaction $ [
                     sampleCard { cardCategories = [Action, Attack] },
                     sampleCard { cardCategories = [Victory] }
                     ])
    assertEqual "reactionRule reaction positive failed"
                True
                (reactionRule ReactionReaction $ [
                     sampleCard { cardCategories = [Action, Attack] },
                     sampleCard { cardCategories = [Action, Reaction] }
                     ])

trasherRuleTests = TestCase $ do
    assertEqual "trasherRule curse negative failed"
                False
                (trasherRule CurseTrasher $ [
                     sampleCard { cardGivesCurses = True },
                     sampleCard { cardTrashesCards = False }
                     ])
    assertEqual "trasherRule curse positive failed"
                True
                (trasherRule CurseTrasher $ [
                     sampleCard { cardGivesCurses = True },
                     sampleCard { cardTrashesCards = True }
                     ])
    assertEqual "trasherRule always negative failed"
                False
                (trasherRule AlwaysTrasher $ [
                     sampleCard { cardTrashesCards = False }
                     ])
    assertEqual "trasherRule always positive failed"
                True
                (trasherRule AlwaysTrasher $ [
                     sampleCard { cardTrashesCards = True }
                     ])
-}


{- Test list -}

hunitTests = TestList []

quickCheckTests = $quickCheckAll

