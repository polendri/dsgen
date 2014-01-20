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


{- Test list -}

hunitTests = TestList [
    TestLabel "actionFilter tests" actionFilterTests,
    TestLabel "complexityFilter tests" complexityFilterTests,
    TestLabel "costVarietyRule tests" costVarietyRuleTests,
    TestLabel "interactivityRule tests" interactivityRuleTests,
    TestLabel "reactionRule tests" reactionRuleTests,
    TestLabel "trasherRule tests" trasherRuleTests
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

