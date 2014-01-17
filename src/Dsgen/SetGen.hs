module Dsgen.SetGen
    (
    -- * Types
    Emphasis(..),
    Filter,
    Rule,
    PlatinumRule(..),
    SheltersRule(..),
    SetGenOptions(..)
    ) where

import Control.Monad
import Data.Array.IO
import System.Random

import Paths_dsgen
import Dsgen.Cards

{- | Set generation emphasis. A set generated with an "emphasized" expansion
will contain at least 4 cards from the emphasized expansion. -}
data Emphasis = NoEmphasis
              | DominionEmphasis
              | IntrigueEmphasis
              | SeasideEmphasis
              | AlchemyEmphasis
              | ProsperityEmphasis
              | CornucopiaEmphasis
              | HinterlandsEmphasis
              | DarkAgesEmphasis
              | GuildsEmphasis

-- | A filter for thinning out sets of 'Card's.
type Filter = [Card] -> [Card]

{- | A function which takes a set of cards and returns whether or not the set
satisfies the rule. -}
type Rule = [Card] -> Bool

-- | Summarizes the possible settings regarding the Platinum card.
data PlatinumRule = NoPlatinum | RandomPlatinum | PlatinumWithColony

-- | Summarizes the possible settings regarding the Shelter cards.
data SheltersRule = NoShelters | RandomShelters | SheltersWithDarkAges

{- | Contains options for customizing the generation of Kingdom card sets -}
data SetGenOptions = SetGenOptions {
    setGenSources :: [CardSource],
    setGenEmphasis :: Emphasis,
    setGenFilters :: [Filter],
    setGenRules :: [Rule],
    setGenRandomColony :: Bool,
    setGenPlatinumRule :: PlatinumRule,
    setGenSheltersRule :: SheltersRule
    }

-- | Contains the results of generating a set of Kingdom cards.
data SetGenResult = SetGenResult {
    setKingdomCards :: [Card],
    setUsesColony :: Bool,
    setUsesPlatinum :: Bool,
    setUsesShelters :: Bool
    }

type SetGenError = String

{- | Generates a list of ten randomly-selected cards, given a list of cards to
choose from and a 'SetGenOptions' to specify how-}
generateSet :: SetGenOptions -> [Card] -> IO (Either SetGenError SetGenResult)
generateSet sgos cs = do
    kcse <- selectKingdomCards cs (setGenSources sgos) (setGenFilters sgos) (setGenRules sgos)
    case kcse of
        Left e    -> return $ Left e
        Right kcs -> do
            return $ Right SetGenResult {
                setKingdomCards = kcs,
                setUsesColony = False,
                setUsesPlatinum = False,
                setUsesShelters = False
            }
  -- where addEmphasisRule NoEmphasis rs = rs
        -- addEmphasisRule em rs = (\cs -> all (\c -> cardSource c == emphasisToSource em) cs) : rs
        -- emphasisToSource DominionEmphasis = Dominion
        -- emphasisToSource IntrigueEmphasis = Intrigue
        -- emphasisToSource SeasideEmphasis = Seaside
        -- emphasisToSource AlchemyEmphasis = Alchemy
        -- emphasisToSource ProsperityEmphasis = Prosperity
        -- emphasisToSource CornucopiaEmphasis = Cornucopia
        -- emphasisToSource HinterlandsEmphasis = Hinterlands
        -- emphasisToSource DarkAgesEmphasis = DarkAges
        -- emphasisToSource GuildsEmphasis = Guilds

selectKingdomCards :: [Card] -> [CardSource] -> [Filter] -> [Rule] -> IO (Either SetGenError [Card])
selectKingdomCards cs ss fs rs = do
    let sourced = foldr (\x y -> filter (\z -> cardSource z == x) y) cs ss
    let filtered = foldr (\x y -> x y) cs fs
    cards <- shuffle filtered
    let pared = fullyPareSet rs cards
    case pared of
        Left s -> return $ Left s
        Right cs -> return $ Right cs

-- decidePlatinum :: PlatinumRule -> Bool -> IO Bool
-- decidePlatinum cb
    -- | NoPlatinum <- return False
    -- | AlwaysPlatinum <- return True
    -- |

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
pareset _ [] = Left "Unable to pare down the set; rules are too strict"
pareSet rs (c:cs) = if satisfiesRules rs cs
                    then Right cs
                    else liftM (c:) (pareSet rs cs)
  where satisfiesRules rs cs = and $ map (\x -> x cs) rs

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

