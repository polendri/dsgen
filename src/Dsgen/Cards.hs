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

data CardType = Attack
              | Reaction
              | Duration
              | Treasure
              | Victory
    deriving (Eq, Read, Show)

data CardComplexity = Low | Medium | High
    deriving (Eq, Read, Show)

data Card = Card {
    cardSource        :: CardSource,
    cardCost          :: Amount,
    cardPotionCost    :: Amount,
    cardTypes         :: [CardType],
    cardPlusCards     :: Amount,
    cardPlusActions   :: Amount,
    cardPlusBuys      :: Amount,
    cardPlusCoins     :: Amount,
    cardGivesCurses   :: Bool,
    cardGainsCards    :: Bool,
    cardTrashesCards  :: Bool,
    cardTrashesItself :: Bool,
    cardComplexity    :: CardComplexity
} deriving (Eq, Show)

type CardError = String

readCardFile :: String -> IO (Either CardError [Card])
readCardFile path = do
    rv <- runErrorT $ do
        -- dominionFilename <- join $ liftIO $ getDataFileName "res/conf/dominionCards.txt"
        -- cp <- join $ liftIO $ readfile emptyCP dominionFilename
        cp <- join $ liftIO $ readfile emptyCP "test.txt"
        let cardNames = sections cp
        foldM (f cp) [] cardNames
    either (\x -> return $ Left $ snd x) (\x -> return $ Right x) rv
  where f cp cs name = do
            -- c <- readCard cp name
            -- return (c:cs)
            return cs 

-- readCard :: MonadError CPError m => ConfigParser -> String -> m Card
-- readCard cp name = do
--    sourceParsed      <- get cp name "source"        -- >>= readWithError
--    costParsed        <- get cp name "cost"          -- >>= readWithError
--    potionCostParsed  <- get cp name "potionCost"    -- >>= readWithError
--    cardTypesParsed   <- get cp name "cardTypes"     -- >>= readWithError
--    plusCardsParsed   <- get cp name "plusCards"     -- >>= readWithError
--    plusActionsParsed <- get cp name "plusActions"   -- >>= readWithError
--    plusBuysParsed    <- get cp name "plusBuys"      -- >>= readWithError
--    plusCoinsParsed   <- get cp name "plusCoins"     -- >>= readWithError
--    givesCurses       <- get cp name "givesCurses"   :: IO Bool
--    gainsCards        <- get cp name "gainsCards"    :: IO Bool
--    trashesCards      <- get cp name "trashesCards"  :: IO Bool
--    trashesItself     <- get cp name "trashesItself" :: IO Bool
--    complexityParsed  <- get cp name "complexity"    -- >>= readWithError
--    return Card {
--        cardSource        = sourceParsed,
--        cardCost          = costParsed,
--        cardPotionCost    = potionCostParsed,
--        cardTypes         = cardTypesParsed,
--        cardPlusCards     = plusCardsParsed,
--        cardPlusActions   = plusActionsParsed,
--        cardPlusBuys      = plusBuysParsed,
--        cardPlusCoins     = plusCoinsParsed,
--        cardGivesCurses   = givesCurses,
--        cardTrashesCards  = trashesCards,
--        cardTrashesItself = trashesItself,
--        cardComplexity    = complexityParsed
--    }
--  where readWithError s = case reads s of
--                              ((p, _):_) -> return p
--                              otherwise  -> throwError (ParseError "Parse error", s)
