import Control.Monad.Error
import Data.List
import System.Random

import Paths_dsgen
import Dsgen.Cards
import Dsgen.SetSelect

main :: IO ()
main = do
    cardFiles <- cardFileNames
    cardse <- runErrorT $ readCardFiles cardFiles
    case cardse of
        Left s -> putStrLn $ "Error: " ++ (show s)
        Right cs -> do
            let ssos = SetSelectOptions {
                           setSelectSources = [
                               Dominion, Intrigue, Seaside, Alchemy, Prosperity,
                               Cornucopia, Hinterlands, DarkAges, Guilds, EnvoyPromo,
                               BlackMarketPromo, StashPromo, WalledVillagePromo,
                               GovernorPromo, Custom],
                           setSelectEmphasis = NoEmphasis,
                           setSelectFilters = [],
                           setSelectRules = [],
                           setSelectManualPicks = [],
                           setSelectCardPool = cs,
                           setSelectColonyAddition = NoColony,
                           setSelectPlatinumAddition = NoPlatinum,
                           setSelectSheltersAddition = NoShelters
                       }
            ssre <- runErrorT $ selectSet ssos
            case ssre of
                Left s -> putStrLn $ "Error: " ++ s
                Right ssr -> putStrLn $ show ssr

{- | Returns a list of filepaths to all predefined card files. This is in the
'IO' monad since it uses cabal's "getDataFileName" function, which maps source
paths to their corresponding packaged location. -}
cardFileNames :: IO [String]
cardFileNames = mapM getPath files
  where getPath s = getDataFileName $ "res/cards/" ++ s ++ ".txt"
        files = ["dominion", "intrigue", "seaside", "alchemy", "prosperity",
                 "cornucopia", "hinterlands", "darkAges", "guilds", "promos",
                 "custom"]

