{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Dsgen.TestSetSelect where

import Control.Monad(liftM)
import qualified Data.Set as Set
import Test.HUnit
import Test.QuickCheck
import qualified Test.QuickCheck.Monadic as QCM
import Test.QuickCheck.All

import Dsgen.Cards
import Dsgen.SetSelect.Internals
import Dsgen.TestCards
import Dsgen.TestHelpers


{- Test definitions -}

prop_actionFilterPositive (c :: Card) =
    elemAny [Treasure, Victory] (cardCategories c) ==> actionFilter c == False

prop_actionFilterNegative (c :: Card) =
    notElemAny [Treasure, Victory] (cardCategories c) ==> actionFilter c == True

prop_complexityFilterPositive (cxo :: ComplexityFilterOption) (c :: Card) =
    cx >= cardComplexity c ==> complexityFilter cxo c == True
  where cx = cplxOptToCplx cxo

prop_complexityFilterNegative (cxo :: ComplexityFilterOption) (c :: Card) =
    cx < cardComplexity c ==> complexityFilter cxo c == False
  where cx = cplxOptToCplx cxo

prop_costVarietyRulePositive (cs :: [Card]) =
    allElems amts (map cardCost cs) ==> costVarietyRule cs == True
  where amts = [FixedAmount 2, FixedAmount 3, FixedAmount 4, FixedAmount 5]

prop_costVarietyRuleNegative (cs :: [Card]) =
    not (allElems amts (map cardCost cs)) ==> costVarietyRule cs == False
  where amts = [FixedAmount 2, FixedAmount 3, FixedAmount 4, FixedAmount 5]

-- Having problems testing list equality on Cards when multiple cards with the same name show up
-- prop_shuffleConsistency (cs :: [Card]) = QCM.monadicIO $ do
--     shuffled <- QCM.run $ shuffle cs
--     QCM.assert $ Set.fromList cs == Set.fromList shuffled


{- Test lists -}

hunitTests = TestList []

quickCheckTests = $quickCheckAll


{- QuickCheck definitions for types -}

instance Arbitrary Emphasis where
    arbitrary = elements [NoEmphasis, RandomEmphasis, DominionEmphasis,
                          IntrigueEmphasis, SeasideEmphasis, AlchemyEmphasis,
                          ProsperityEmphasis, CornucopiaEmphasis, HinterlandsEmphasis,
                          DarkAgesEmphasis, GuildsEmphasis]

instance Arbitrary ComplexityFilterOption where
    arbitrary = elements [LowComplexityOnly,
                          MediumComplexityOrLower,
                          HighComplexityOrLower]

instance Arbitrary ReactionRuleOption where
    arbitrary = elements [RequireMoat, RequireBlocker, RequireReaction]

instance Arbitrary TrasherRuleOption where
    arbitrary = elements [TrasherWithCurse, AlwaysTrasher]

instance Arbitrary ColonyAdditionOption where
    arbitrary = elements [NoColony, RandomColony]

instance Arbitrary PlatinumAdditionOption where
    arbitrary = elements [NoPlatinum, RandomPlatinum, PlatinumWithColony]

instance Arbitrary SheltersAdditionOption where
    arbitrary = elements [NoShelters, SheltersWithDarkAges, RandomShelters]


{- Helper functions -}

-- | Converts a 'ComplexityFilterOption' object to a 'CardComplexity' object.
cplxOptToCplx :: ComplexityFilterOption -> CardComplexity
cplxOptToCplx cxo =
    case cxo of
        LowComplexityOnly       -> Low
        MediumComplexityOrLower -> Medium
        HighComplexityOrLower   -> High

