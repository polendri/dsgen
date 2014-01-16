{-# LANGUAGE FlexibleContexts #-}

module Dsgen.Cards where

import Data.ConfigFile
import Control.Monad.Error

import Paths_dsgen

data CardSource = Dominion
                | Intrigue
                | Seaside
                | Alchemy
                | Prosperity
                | Cornucopia
                | Hinterlands
                | DarkAges
                | Guilds
                | EnvoyPromo
                | BlackMarketPromo
                | StashPromo
                | WalledVillagePromo
                | GovernorPromo
                | Custom
    deriving (Eq, Read, Show)

data Amount = FixedAmount Int | Variable
    deriving (Eq, Show)

instance Read Amount where
    readsPrec _ s = if s == "Variable"
                    then [(Variable, "")]
                    else case reads s :: [(Int, String)] of
                             ((i, s'):_) -> [(FixedAmount i, s')]
                             otherwise   -> []

data CardCategory = Action
                  | Attack
                  | Reaction
                  | Duration
                  | Treasure
                  | Victory
                  | Looter
    deriving (Eq, Read, Show)

data CardComplexity = Low | Medium | High
    deriving (Eq, Read, Show)

data Card = Card {
    cardSource        :: CardSource,
    cardCost          :: Amount,
    cardPotionCost    :: Amount,
    cardCategories    :: [CardCategory],
    cardPlusCards     :: Amount,
    cardPlusActions   :: Amount,
    cardPlusBuys      :: Amount,
    cardPlusCoins     :: Amount,
    cardGivesCurses   :: Bool,
    cardGainsCards    :: Bool,
    cardTrashesCards  :: Bool,
    cardTrashesItself :: Bool,
    cardInteractive   :: Bool,
    cardComplexity    :: CardComplexity
} deriving (Eq, Show)

cardFileNames :: IO [String]
cardFileNames = mapM getPath files
  where getPath s = getDataFileName $ "res/cards/" ++ s ++ ".txt"
        files = ["dominion", "intrigue", "seaside", "alchemy", "prosperity",
                 "cornucopia", "hinterlands", "darkages", "guilds", "custom"]

readCardFiles :: ErrorT CPError IO [Card]
readCardFiles = do
    cfns <- liftIO cardFileNames
    liftM concat $ mapM readCardFile cfns

readCardFile :: String -> ErrorT CPError IO [Card]
readCardFile path = do
    cp <- join $ liftIO $ readfile emptyCP path
    let cp' = cp { optionxform = id }
    let cardNames = sections cp'
    foldM (f cp') [] cardNames
  where f cp cs name = readCard cp name >>= \c -> return (c:cs)

readCard :: MonadError CPError m => ConfigParser -> SectionSpec -> m Card
readCard cp name = do
    source        <- get cp name "source"        >>= readWithError
    cost          <- get cp name "cost"          >>= readWithError
    potionCost    <- get cp name "potionCost"    >>= readWithError
    categories    <- get cp name "categories"    >>= readWithError
    plusCards     <- get cp name "plusCards"     >>= readWithError
    plusActions   <- get cp name "plusActions"   >>= readWithError
    plusBuys      <- get cp name "plusBuys"      >>= readWithError
    plusCoins     <- get cp name "plusCoins"     >>= readWithError
    givesCurses   <- get cp name "givesCurses"
    gainsCards    <- get cp name "gainsCards"
    trashesCards  <- get cp name "trashesCards"
    trashesItself <- get cp name "trashesItself"
    interactive   <- get cp name "interactive"
    complexity    <- get cp name "complexity"
    return Card {
        cardSource        = source,
        cardCost          = cost,
        cardPotionCost    = potionCost,
        cardCategories    = categories,
        cardPlusCards     = plusCards,
        cardPlusActions   = plusActions,
        cardPlusBuys      = plusBuys,
        cardPlusCoins     = plusCoins,
        cardGivesCurses   = givesCurses,
        cardGainsCards    = gainsCards,
        cardTrashesCards  = trashesCards,
        cardTrashesItself = trashesItself,
        cardInteractive   = interactive,
        cardComplexity    = complexity
        }
  where readWithError s = case reads s of
                             ((p, _):_) -> return p
                             otherwise  -> throwError (ParseError "Parse error", s)

