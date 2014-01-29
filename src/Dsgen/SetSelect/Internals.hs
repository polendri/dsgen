module Dsgen.SetSelect.Internals where

import Control.Monad(liftM)
import Control.Monad.Error
import Control.Monad.Trans(liftIO)
import Data.Array.IO
import Data.List((\\))
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

-- | A filter for thinning out lists of 'Card's.
type Filter = Card -> Bool

-- | Defines options for the complexity rule.
data ComplexityFilterOption = LowComplexityOnly
                            | MediumComplexityOrLower
                            | HighComplexityOrLower
  deriving (Eq, Ord, Read, Show)

{- | A 'Rule' takes a list of cards and decides whether or not the list
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

-- | Defines options for the Colony/Platinum addition.
data ColPlatAdditionOption = NoColPlat | RandomColPlat
  deriving (Eq, Ord, Read, Show)

-- | Defines options for the Platinum addition.
data PlatinumAdditionOption = NoPlatinum | RandomPlatinum | PlatinumWithColony
  deriving (Eq, Ord, Read, Show)

-- | Defines options for the Shelters addition.
data SheltersAdditionOption = NoShelters | SheltersWithDarkAges | RandomShelters
  deriving (Eq, Ord, Read, Show)

{- | Contains options for customizing the selection of Kingdom card sets -}
data SetSelectOptions = SetSelectOptions {
    ssoPickCount        :: Int,
    ssoPool             :: [Card],
    ssoManualPicks      :: [Card],
    ssoSources          :: [CardSource],
    ssoEmphasis         :: Emphasis,
    ssoFilters          :: [Filter],
    ssoRules            :: [Rule],
    ssoColPlatAddition  :: ColPlatAdditionOption,
    ssoSheltersAddition :: SheltersAdditionOption
    }

-- | Contains the results of selecting a set of Kingdom cards.
data SetSelectResult = SetSelectResult {
    ssrKingdomCards :: [Card],
    ssrUsesColPlat  :: Bool,
    ssrUsesShelters :: Bool
    }
  deriving (Show)

-- | Type synonym for errors returned by 'selectSet'
type SetSelectError = String


{- Filters -}

-- | Filters out Treasure and Victory cards.
actionFilter :: Filter
actionFilter c = (notElem Treasure (cardCategories c)) && (notElem Victory (cardCategories c))

-- | Filters out cards of complexity greater than the provided value.
complexityFilter :: ComplexityFilterOption -> Filter
complexityFilter cx c = cardComplexity c <= cx'
  where cx' = case cx of
                  LowComplexityOnly       -> Low
                  MediumComplexityOrLower -> Medium
                  HighComplexityOrLower   -> High


{- Rules -}

{- | Requires that the list contain at least one card each of values 2, 3, 4 and
5. -}
costVarietyRule :: Rule
costVarietyRule cs =
    (any (\c -> cardCost c == FixedAmount 2) cs) &&
    (any (\c -> cardCost c == FixedAmount 3) cs) &&
    (any (\c -> cardCost c == FixedAmount 4) cs) &&
    (any (\c -> cardCost c == FixedAmount 5) cs)

-- | Requires that the list contain at least the given number of interactive cards.
interactivityRule :: Int -> Rule
interactivityRule n cs = (length $ filter cardInteractive cs) >= n

{- | Requires that the list contain one of the requested type of card, when an
attack card is present in the list. -}
reactionRule :: ReactionRuleOption -> Rule
reactionRule rro cs =
    if any (\c -> elem Attack $ cardCategories c) cs
    then case rro of
        RequireMoat     -> any (\c -> cardName c == "Moat") cs
        RequireBlocker  -> any cardBlocksAttacks cs
        RequireReaction -> any (\c -> elem Reaction $ cardCategories c) cs
    else True

{- | Requires that the list contain a trasher card under the requested
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
that a list contain at least 4 cards from the emphasized set. 'Nothing' is
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
colPlatAddition :: ColPlatAdditionOption -> IO Bool
colPlatAddition ca = case ca of
    NoColPlat     -> return False
    RandomColPlat -> randomIO :: IO Bool

{- Decides randomly whether or not to include Shelters. -}
sheltersAddition :: SheltersAdditionOption -> Bool -> IO Bool
sheltersAddition sa da =
    case sa of
        NoShelters           -> return False
        RandomShelters       -> randomIO :: IO Bool
        SheltersWithDarkAges -> return da


{- Selection Functions -}

{- | Chooses cards to supplement an initial list of cards such that it
contains 10 cards total and satisfies all provided rules. Also decides on
additions such as Colonies or Shelters. -}
selectSet :: SetSelectOptions -- ^ Defines options for how to select a list.
          -> ErrorT SetSelectError IO SetSelectResult
selectSet ssos = do
    let rules  = ssRules ssos
    let rules' = maybe rules (:rules) $ mkEmphasisRule (ssEmphasis ssos)
    kcs <- if length (ssManualPicks ssos) >= ssPickCount ssos
           then return []
           else selectKingdomCards (ssPickCount ssos)
                                   (ssPool ssos)
                                   (ssManualPicks ssos)
                                   (ssSources ssos)
                                   (ssFilters ssos)
                                   rules'
    useColPlat  <- liftIO $ colPlatAddition $ ssColPlatAddition ssos
    useShelters <- liftIO $ sheltersAddition (ssSheltersAddition ssos) (ssEmphasis ssos == DarkAgesEmphasis)

    return SetSelectResult {
        ssrKingdomCards = kcs,
        ssrUsesColPlat  = useColPlat,
        ssrUsesShelters = useShelters
    }

{- | Chooses cards to supplement an initial list of cards such that it contains
10 cards total. The set of 10 satisfies all rules provided, and the
supplemented cards satisfy the card, cardsource, and filter restrictions
provided. -}
selectKingdomCards :: Int            -- ^ The number of cards to pick.
                   -> [Card]         -- ^ The initial selection of cards to build onto.
                   -> [Card]         -- ^ The pool of cards to choose from.
                   -> [CardSource]   -- ^ The list of card sources to filter on.
                   -> [Filter]       -- ^ The list of filters to apply to the pool.
                   -> [Rule]         -- ^ The list of rules to apply when choosing a set.
                   -> ErrorT SetSelectError IO [Card]
selectKingdomCards n cs pool ss fs rs = do
    return pool
    let sourced = filter (\x -> elem (cardSource x) ss) pool
    let filtered = foldr filter sourced fs
    if length filtered < n
     then throwError $ "Not enough sources selected; less than " ++
                       (show n) ++
                       " cards left after filtering."
     else do
         cards <- liftIO $ shuffle filtered
         let pared = fullyPareSet rs cs cards
         case pared of
             Left s -> throwError s
             Right cs' -> if length cs' > n
                          then throwError $ "Rules are too strict; unable to find " ++
                                            (show n) ++
                                            " cards which satisfy the selected requirements."
                          else return cs'

{- | Given a list of rules, a list of preset cards, and a list of cards to
choose from, reduces the choice list down such that there are n cards total,
and returns the cards chosen. -}
fullyPareSet :: Int -> [Rule] -> [Card] -> [Card] -> Either String [Card]
fullyPareSet n rs pcs cs
    | length cs <= n = Right cs
    | otherwise      = pareSet rs pcs cs >>= fullyPareSet rs pcs

{- | Removes one card from a list of cards, such that the provided list of
rules are all still satisfied by the new, smaller list. -}
pareSet :: [Rule] -> [Card] -> [Card] -> Either String [Card]
pareSet rs pcs cs = pareSetHelper rs pcs [] cs
  where pareSetHelper _ _ _ [] = Left "Unable to pare down the set; rules are too strict"
        pareSetHelper rs pcs xcs (c:cs) =
            if satisfiesRules rs $ xcs ++ cs
            then Right cs
            else liftM (c:) (pareSetHelper rs pcs (c:xcs) cs)
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

