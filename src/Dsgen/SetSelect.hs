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
import Control.Monad.Error
import Control.Monad.Trans(liftIO)
import Data.Array.IO
import qualified Data.Foldable as F
import Data.Set(Set)
import qualified Data.Set as Set
import System.Random

import Dsgen.Cards

{- Types -}

{- | Set selection emphasis. A set selected with an "emphasized" expansion
will contain a consistently higher proportion of cards from that expansion. -}
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
  deriving (Eq, Ord, Read, Show)

-- | A filter for thinning out sets of 'Card's.
type Filter = Card -> Bool

-- | Defines options for the complexity rule.
data ComplexityFilterOption = LowComplexityOnly
                            | MediumComplexityOrLower
                            | HighComplexityOrLower
  deriving (Eq, Ord, Read, Show)

{- | A 'Rule' takes a set of cards and decides whether or not the set
satisfies some kind of criteria. -}
type Rule = [Card] -> Bool

-- | Defines options for the reaction rule.
data ReactionRuleOption = RequireMoat | RequireBlocker | RequireReaction
  deriving (Eq, Ord, Read, Show)

-- | Defines options for the trasher rule.
data TrasherRuleOption  = TrasherWithCurse | AlwaysTrasher
  deriving (Eq, Ord, Read, Show)

-- | Determines whether or not to include an addition.
type Addition = IO Bool

-- | Defines options for the Colony addition.
data ColonyAdditionOption = NoColony | RandomColony
  deriving (Eq, Ord, Read, Show)

-- | Defines options for the Platinum addition.
data PlatinumAdditionOption = NoPlatinum | RandomPlatinum | PlatinumWithColony
  deriving (Eq, Ord, Read, Show)

-- | Defines options for the Shelters addition.
data SheltersAdditionOption = NoShelters | SheltersWithDarkAges | RandomShelters
  deriving (Eq, Ord, Read, Show)

{- | Contains options for customizing the selection of Kingdom card sets -}
data SetSelectOptions = SetSelectOptions {
    setSelectSources :: Set CardSource,
    setSelectEmphasis :: Emphasis,
    setSelectFilters :: [Filter],
    setSelectRules :: [Rule],
    setSelectColonyAddition :: ColonyAdditionOption,
    setSelectPlatinumAddition :: PlatinumAdditionOption,
    setSelectSheltersAddition :: SheltersAdditionOption
    }

-- | Typeclass for types which can be converted into 'SetSelectOptions'
class SetOptionable a where
    toSetSelectOptions :: a -> SetSelectOptions

-- | Contains the results of selecting a set of Kingdom cards.
data SetSelectResult = SetSelectResult {
    setKingdomCards :: Set Card,
    setUsesColony   :: Bool,
    setUsesPlatinum :: Bool,
    setUsesShelters :: Bool
    }
  deriving (Show)

-- | Type synonym for errors returned by 'selectSet'
type SetSelectError = String


{- Filters -}

-- | Filters out Treasure and Victory cards.
actionFilter :: Filter
actionFilter c = (F.notElem Treasure (cardCategories c)) && (F.notElem Victory (cardCategories c))

-- | Filters out cards of complexity greater than the provided value.
complexityFilter :: ComplexityFilterOption -> Filter
complexityFilter cx c = cardComplexity c <= cx'
  where cx' = case cx of
                  LowComplexityOnly       -> Low
                  MediumComplexityOrLower -> Medium
                  HighComplexityOrLower   -> High


{- Rules -}

{- | Requires that the set contain at least one card each of values 2, 3, 4 and
5. -}
costVarietyRule :: Rule
costVarietyRule cs =
    (F.any (\c -> cardCost c == FixedAmount 2) cs) &&
    (F.any (\c -> cardCost c == FixedAmount 3) cs) &&
    (F.any (\c -> cardCost c == FixedAmount 4) cs) &&
    (F.any (\c -> cardCost c == FixedAmount 5) cs)

-- | Requires that the set contain at least the given number of interactive cards.
interactivityRule :: Int -> Rule
interactivityRule n cs = (length $ filter cardInteractive cs) >= n

{- | Requires that the set contain one of the requested type of card, when an
attack card is present in the set. -}
reactionRule :: ReactionRuleOption -> Rule
reactionRule rro cs =
    if any (\c -> elem Attack $ cardCategories c) cs
    then case rro of
        RequireMoat     -> any (\c -> cardName c == "Moat") cs
        RequireBlocker  -> any cardBlocksAttacks cs
        RequireReaction -> any (\c -> elem Reaction $ cardCategories c) cs
    else True

{- | Requires that the set contain a trasher card under the requested
circumstances. -}
trasherRule :: TrasherRuleOption -> Rule
trasherRule tro cs =
    case tro of
        TrasherWithCurse -> if any cardGivesCurses cs
                            then trasherExists cs
                            else True
        AlwaysTrasher    -> trasherExists cs
  where trasherExists cs = any cardTrashesCards cs

{- | Generates an emphasis 'Rule' from a provided emphasis, which requires
that a set contain at least 4 cards from the emphasized set. 'Nothing' is
return if 'NoEmphasis' is provided, however. -}
mkEmphasisRule :: Emphasis -> Maybe Rule
mkEmphasisRule e =
    if e == NoEmphasis
    then Nothing
    else Just $ \cs -> emphasisMinCount <= (length $ filter (\c -> cardSource c == emphasisToSource e) cs)
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

{- | Selects a set of ten randomly-selected cards, given a set of cards to
choose from, a set of cards to start with, and a 'SetSelectOptions' to specify
how to do the choosing. -}
selectSet :: SetSelectOptions -- ^ Defines options for how to select a set.
          -> Set Card           -- ^ A set of input cards to begin with.
          -> Set Card           -- ^ A pool of cards to choose from.
          -> ErrorT SetSelectError IO SetSelectResult
selectSet sgos cs pool = do
    let rules  = setSelectRules sgos
    let rules' = maybe rules (:rules) $ mkEmphasisRule (setSelectEmphasis sgos)
    kcs <- if Set.size cs >= 10
           then return cs
           else selectKingdomCards cs
                                   pool
                                   (setSelectSources sgos)
                                   (setSelectFilters sgos)
                                   rules'
    useColony   <- liftIO $ colonyAddition $ setSelectColonyAddition sgos
    usePlatinum <- liftIO $ platinumAddition (setSelectPlatinumAddition sgos) useColony
    useShelters <- liftIO $ sheltersAddition (setSelectSheltersAddition sgos) (setSelectEmphasis sgos == DarkAgesEmphasis)
    return SetSelectResult {
        setKingdomCards = kcs,
        setUsesColony   = useColony,
        setUsesPlatinum = usePlatinum,
        setUsesShelters = useShelters
    }

{- | Selects Kingdom cards from the provided pool of cards, such that the
selected cards satisfy all the provided source, filter, and rule restrictions.
In addition, a set of cards can be specfied as a starting point, and rules
will be applied as though those cards were a part of the selected set. -}
selectKingdomCards :: Set Card       -- ^ The initial selection of cards to build onto.
                   -> Set Card       -- ^ The pool of cards to choose from.
                   -> Set CardSource -- ^ The set of card sources to filter on.
                   -> [Filter]       -- ^ The list of filters to apply to the pool.
                   -> [Rule]         -- ^ The list of rules to apply when choosing a set.
                   -> ErrorT SetSelectError IO (Set Card)
selectKingdomCards cs pool ss fs rs = do
    let sourced = Set.filter (\x -> F.elem (cardSource x) ss) pool
    let filtered = F.foldr Set.filter sourced fs
    if Set.size filtered < 10
     then throwError "Not enough sources selected; less than 10 cards left after filtering."
     else do
         cards <- liftIO $ shuffle $ Set.toList filtered
         let pared = fullyPareSet rs cards
         case pared of
             Left s -> throwError s
             Right cs' -> if Set.size pool > 10
                          then throwError "Rules are too strict; unable to reduce down to 10 cards."
                          else return $ Set.fromList cs'

{- | Given a list of cards and a set of rules, reduces the list down to 10
cards which still satisfy all the rules and returns that list, or an error
string if it is not possible to do so. -}
fullyPareSet :: [Rule] -> [Card] -> Either String [Card]
fullyPareSet rs cs
    | length cs <= 10 = Right cs
    | otherwise       = pareSet rs cs >>= fullyPareSet rs

{- | Removes one card from a list of cards, such that the provided set of
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

