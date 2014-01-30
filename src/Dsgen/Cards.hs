{-# LANGUAGE FlexibleContexts #-}

module Dsgen.Cards
    (
    -- * Types
    Card(..),
    CardSource(..),
    Amount(..),
    CardCategory(..),
    CardComplexity(..),

    -- * Card Readers
    readCardFiles, readCardFile, readCard,

    -- * Utility Functions
    showCardName
    ) where

import Data.ConfigFile
import Control.Monad.Error

-- | Represents a Dominion Kingdom card.
data Card = Card {
    cardName          :: String,         -- ^ The name of the card, as it appears on the card.
    cardSource        :: CardSource,     -- ^ The expansion from which the card originates.
    cardCost          :: Amount,         -- ^ The cost of the card, in coins.
    cardPotionCost    :: Amount,         -- ^ The cost of the card, in potions.
    cardCategories    :: [CardCategory], -- ^ The categories to which the card belongs.
    cardPlusCards     :: Amount,         -- ^ The net amount of cards gained by playing this card.
    cardPlusActions   :: Amount,         -- ^ The amount of actions gained by playing this card.
    cardPlusBuys      :: Amount,         -- ^ The amount of buys gained by playing this card.
    cardPlusCoins     :: Amount,         -- ^ The amount of coins gained by playing this card.
    cardBlocksAttacks :: Bool,           -- ^ True iff the card can nullify attacks
    cardGivesCurses   :: Bool,           -- ^ True iff the card is able to give curses.
    cardGainsCards    :: Bool,           -- ^ True iff the card is able to gain cards for its user.
    cardTrashesCards  :: Bool,           -- ^ True iff the card is able to trash cards for its user.
    cardTrashesItself :: Bool,           -- ^ True iff the card is able to have itself trashed.
    cardInteractive   :: Bool,           -- ^ True iff playing the card involves interacting with other players.
    cardComplexity    :: CardComplexity  -- ^ A subjective measure of how complicated the card is to use
} deriving (Eq, Show)

instance Ord Card where
    compare c1 c2 = compare (cardName c1) (cardName c2)

{- | The source of a card, i.e. the expansion from which it originates. Since
promo cards are typically acquired individually, each has its own source.
'Custom' represents any unofficial card which may have been added manually. -}
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
    deriving (Eq, Ord, Read, Show)

-- | A data type which represents either a fixed integer, or 'Variable'.
data Amount = FixedAmount Int | Variable
    deriving (Eq, Show)

instance Read Amount where
    readsPrec _ s = if s == "Variable"
                    then [(Variable, "")]
                    else case reads s :: [(Int, String)] of
                             ((i, s'):_) -> [(FixedAmount i, s')]
                             otherwise   -> []

displayAmount :: Amount -> String
displayAmount Variable = "?"
displayAmount (FixedAmount n) = show n

{- | The categories that a card can belong to. There are more categories than
these, such as "Knight" and "Ruin", but these are not relevant to choosing sets
of Kingdom cards. -}
data CardCategory = Action
                  | Attack
                  | Reaction
                  | Duration
                  | Treasure
                  | Victory
                  | Looter
    deriving (Eq, Read, Show)

{- | The subjective complexity of the card. This is a measure of how
complicated it is to follow the instructions of a card. Roughly speaking,

 * 'Low' is intended to represent cards with at most one simple instruction
   beyond the usual +1/+2/etc indicators;

 * 'Medium' is intended to represent cards which may have multiple instructions
   or decisions, but which are still relatively easy to follow;

 * 'High' is intended to represent cards which either have long, multi-step
   instructions which are tricky to understand or follow, or involve unique or
   otherwise non-standard features, such as mats, tokens, trashing or
   overpayment effects, etc.
-}
data CardComplexity = Low | Medium | High
    deriving (Eq, Ord, Read, Show)

{- | Given a card, returns a string containing the card's name followed by its
source (in brackets) -}
showCardName :: Card -> String
showCardName c = (cardName c) ++ " (" ++ (show $ cardSource c) ++ ")"

-- | Reads all specified card files, returning them in a single list
readCardFiles :: [String] -> ErrorT CPError IO [Card]
readCardFiles cfns = liftM concat $ mapM readCardFile cfns

-- | Reads the contents of a single card file into a list of 'Card's.
readCardFile :: String -> ErrorT CPError IO [Card]
readCardFile path = do
    cp <- join $ liftIO $ readfile emptyCP path
    let cp' = cp { optionxform = id }
    let cardNames = sections cp'
    foldM (f cp') [] cardNames
  where f cp cs name = readCard cp name >>= \c -> return (c:cs)

-- | Reads a single card from a 'ConfigParser', given the name of the card
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
    blocksAttacks <- get cp name "blocksAttacks"
    givesCurses   <- get cp name "givesCurses"
    gainsCards    <- get cp name "gainsCards"
    trashesCards  <- get cp name "trashesCards"
    trashesItself <- get cp name "trashesItself"
    interactive   <- get cp name "interactive"
    complexity    <- get cp name "complexity"
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
  where readWithError s = case reads s of
                             ((p, _):_) -> return p
                             otherwise  -> throwError (ParseError $ "Parse error in section \"" ++ name ++ "\"", s)

