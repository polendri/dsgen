module Dsgen.SetSelect
    (
    -- * Types
    Emphasis(..),
    Filter,
    ComplexityFilterOption(..),
    Rule,
    ReactionRuleOption(..),
    TrasherRuleOption(..),
    Addition,
    ColonyAdditionOption(..),
    PlatinumAdditionOption(..),
    SheltersAdditionOption(..),
    SetSelectOptions(..),
    SetOptionable(..),
    SetSelectResult(..),
    SetSelectError,

    -- * Filters
    actionFilter,
    complexityFilter,

    -- * Rules
    costVarietyRule,
    interactivityRule,
    reactionRule,
    trasherRule,

    -- * Set selection functions
    selectSet
    ) where

import Control.Monad
import Data.Array.IO
import System.Random

import Dsgen.Cards

{- Types -}

{- | Set eneration emphasis. A set selected with an "emphasized" expansion
will contain at least 4 cards from the emphasized expansion. -}
data Emphasis = NoEmphasis
              | RandomEmphasis
              | DominionEmphasis
              | IntrigueEmphasis
              | SeasideEmphasis
              | AlchemyEmphasis
              | ProsperityEmphasis
              | CornucopiaEmphasis
              | HinterlandsEmphasis
              | DarkAgesEmphasis
              | GuildsEmphasis
    deriving (Eq)

-- | A filter for thinning out sets of 'Card's.
type Filter = Card -> Bool

-- | Defines options for the complexity rule.
data ComplexityFilterOption = LowComplexityFilterOption | MediumComplexityFilterOption | HighComplexityFilterOption

{- | A function which takes a set of cards and returns whether or not the set
satisfies the rule. -}
type Rule = [Card] -> Bool

-- | Defines options for the reaction rule.
data ReactionRuleOption = MoatReaction | BlockerReaction | ReactionReaction

-- | Defines options for the trasher rule.
data TrasherRuleOption  = CurseTrasher | AlwaysTrasher

-- | Determines whether or not to include an addition.
type Addition = IO Bool

-- | Defines options for the Colony addition.
data ColonyAdditionOption = NoColony | RandomColony

-- | Defines options for the Platinum addition.
data PlatinumAdditionOption = NoPlatinum | RandomPlatinum | PlatinumWithColony

-- | Defines options for the Shelters addition.
data SheltersAdditionOption = NoShelters | SheltersWithDarkAges | RandomShelters

{- | Contains options for customizing the selection of Kingdom card sets -}
data SetSelectOptions = SetSelectOptions {
    setSelectSources :: [CardSource],
    setSelectEmphasis :: Emphasis,
    setSelectFilters :: [Filter],
    setSelectRules :: [Rule],
    setSelectColonyAddition :: ColonyAdditionOption,
    setSelectPlatinumAddition :: PlatinumAdditionOption,
    setSelectSheltersAddition :: SheltersAdditionOption
    }

-- | typeclass for types which can be converted into 'SetSelectOptions'
class SetOptionable a where
    toSetSelectOptions :: a -> SetSelectOptions

-- | Contains the results of selecting a set of Kingdom cards.
data SetSelectResult = SetSelectResult {
    setKingdomCards :: [Card],
    setUsesColony :: Bool,
    setUsesPlatinum :: Bool,
    setUsesShelters :: Bool
    }

type SetSelectError = String


{- Filters -}

-- | Filters out Treasure and Victory cards.
actionFilter :: Filter
actionFilter c = (notElem Treasure (cardCategories c)) && (notElem Victory (cardCategories c))

-- | Filters out cards of complexity greater than the provided value.
complexityFilter :: ComplexityFilterOption -> Filter
complexityFilter cx c = cardComplexity c <= cx'
  where cx' = case cx of
                  LowComplexityFilterOption -> Low
                  MediumComplexityFilterOption -> Medium
                  HighComplexityFilterOption -> High


{- Rules -}

{- | Requires that the set contain at least one card each of values 2, 3, 4 and
5. -}
costVarietyRule :: Rule
costVarietyRule cs =
    (any (\c -> cardCost c == FixedAmount 2) cs) &&
    (any (\c -> cardCost c == FixedAmount 3) cs) &&
    (any (\c -> cardCost c == FixedAmount 4) cs) &&
    (any (\c -> cardCost c == FixedAmount 5) cs)

-- | Requires that the set contain the given number of interactive cards.
interactivityRule :: Int -> Rule
interactivityRule n cs = (length $ filter cardInteractive cs) >= n

{- | Requires that the set contain one of the requested type of card, when an
attack card is present in the set. -}
reactionRule :: ReactionRuleOption -> Rule
reactionRule rr cs =
    if any (\c -> elem Attack $ cardCategories c) cs
    then case rr of
        MoatReaction -> any (\c -> cardName c == "Moat") cs
        BlockerReaction -> any cardBlocksAttacks cs
        ReactionReaction -> any (\c -> elem Reaction $ cardCategories c) cs
    else True

{- | Requires that the set contain a trasher card under the provided
circumstances. -}
trasherRule :: TrasherRuleOption -> Rule
trasherRule tr cs =
    case tr of
        CurseTrasher -> if any cardGivesCurses cs
                        then trasherExists cs
                        else True
        AlwaysTrasher -> trasherExists cs
  where trasherExists cs = any cardTrashesCards cs

-- | Adds the appropriate emphasis rule to a list of rules, if any.
addEmphasisRule :: Emphasis -> [Rule] -> [Rule]
addEmphasisRule e rs = case e of
                   NoEmphasis -> rs
                   otherwise  -> (\cs -> length (filter (\c -> cardSource c == emphasisToSource e) cs) >= emphasisMinCount) : rs
  where emphasisMinCount = 4
        emphasisToSource e = case e of
            DominionEmphasis    -> Dominion
            IntrigueEmphasis    -> Intrigue
            SeasideEmphasis     -> Seaside
            AlchemyEmphasis     -> Alchemy
            ProsperityEmphasis  -> Prosperity
            CornucopiaEmphasis  -> Cornucopia
            HinterlandsEmphasis -> Hinterlands
            DarkAgesEmphasis    -> DarkAges
            GuildsEmphasis      -> Guilds


{- Additions -}

{- Decides whether or not to include Colony. -}
colonyAddition :: ColonyAdditionOption -> IO Bool
colonyAddition ca = case ca of
    NoColony     -> return False
    RandomColony -> randomIO :: IO Bool

{- Decides whether or not to include Platinum. -}
platinumAddition :: PlatinumAdditionOption -> Bool -> IO Bool
platinumAddition pa c =
    case pa of
        NoPlatinum         -> return False
        RandomPlatinum     -> randomIO :: IO Bool
        PlatinumWithColony -> return c

{- Decides randomly whether or not to include Shelters. -}
sheltersAddition :: SheltersAdditionOption -> Bool -> IO Bool
sheltersAddition sa da =
    case sa of
        NoShelters           -> return False
        RandomShelters       -> randomIO :: IO Bool
        SheltersWithDarkAges -> return da


{- Selection Functions -}

{- | Selects a list of ten randomly-selected cards, given a list of cards to
choose from and a 'SetSelectOptions' to specify how -}
selectSet :: SetSelectOptions -> [Card] -> IO (Either SetSelectError SetSelectResult)
selectSet sgos cs = do
    let rules = addEmphasisRule (setSelectEmphasis sgos) (setSelectRules sgos)
    kcse <- selectKingdomCards cs (setSelectSources sgos) (setSelectFilters sgos) rules
    case kcse of
        Left e    -> return $ Left e
        Right kcs -> do
            useColony <- colonyAddition $ setSelectColonyAddition sgos
            usePlatinum <- platinumAddition (setSelectPlatinumAddition sgos) useColony
            useShelters <- sheltersAddition (setSelectSheltersAddition sgos) (setSelectEmphasis sgos == DarkAgesEmphasis)
            return $ Right SetSelectResult {
                setKingdomCards = kcs,
                setUsesColony = useColony,
                setUsesPlatinum = usePlatinum,
                setUsesShelters = useShelters
            }

{- | Selects ten Kingdom cards from the provided set, after applying the
CardSource and other filters, such that the set of 10 satisfies all the
provided rules. If this is not possible, an error is returned. -}
selectKingdomCards :: [Card] -> [CardSource] -> [Filter] -> [Rule] -> IO (Either SetSelectError [Card])
selectKingdomCards cs ss fs rs = do
    let sourced = filter (\x -> elem (cardSource x) ss) cs
    let filtered = foldr filter sourced fs
    if length filtered < 10
     then return $ Left "Not enough sources selected; less than 10 cards left after filtering."
     else do
         cards <- shuffle filtered
         let pared = fullyPareSet rs cards
         case pared of
             Left s -> return $ Left s
             Right cs -> if length cs > 10
                         then return $ Left "Rules are too strict; unable to reduce down to 10 cards."
                         else return $ Right cs

{- | Given a list of cards and a set of rules, reduces the list down to 10
cards which still satisfy all the rules and returns that list, or an error
string if it is not possible to do so. -}
fullyPareSet :: [Rule] -> [Card] -> Either String [Card]
fullyPareSet rs cs
    | length cs <= 10 = Right cs
    | otherwise       = pareSet rs cs >>= fullyPareSet rs

{- | Removes one card from a list of cards, such that the provided list of
rules are all still satisfied by the new, smaller list. -}
pareSet :: [Rule] -> [Card] -> Either String [Card]
pareSet rs cs = pareSetHelper rs [] cs
  where pareSetHelper _ _ [] = Left "Unable to pare down the set; rules are too strict"
        pareSetHelper rs xcs (c:cs) =
            if satisfiesRules rs $ xcs ++ cs
            then Right cs
            else liftM (c:) (pareSetHelper rs (c:xcs) cs)
        satisfiesRules rs cs = all (\x -> x cs) rs

{- | Randomly shuffle a list.
Obtained from http://www.haskell.org/haskellwiki/Random_shuffle -}
shuffle :: [a] -> IO [a]
shuffle xs = do
    ar <- newArray n xs
    forM [1..n] $ \i -> do
        j <- randomRIO (i,n)
        vi <- readArray ar i
        vj <- readArray ar j
        writeArray ar j vi
        return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs

