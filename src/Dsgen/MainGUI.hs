import Control.Monad.Error
import Control.Monad.Trans(liftIO)
import qualified Data.Foldable as F
import Data.List
import Graphics.UI.Gtk
import System.Random

import Paths_dsgen
import Dsgen.Cards
import Dsgen.GUIState
import Dsgen.SetSelect

-- | Builds a 'SetSelectOptions' object based on the GUI state and a card pool.
mkSetSelectOptions :: GUIState -> [Card] -> SetSelectOptions
mkSetSelectOptions gst cs = SetSelectOptions {
    setSelectPool = cs, -- TODO
    setSelectManualPicks = [], -- TODO
    setSelectSources = sources,
    setSelectEmphasis = emphasis,
    setSelectFilters = filters,
    setSelectRules = rules,
    setSelectColonyAddition = colony,
    setSelectPlatinumAddition = platinum,
    setSelectSheltersAddition = shelters
    }
  where sources = concat [
            (if guiDominionChecked      gst then [Dominion] else []),
            (if guiIntrigueChecked      gst then [Intrigue] else []),
            (if guiAlchemyChecked       gst then [Alchemy] else []),
            (if guiSeasideChecked       gst then [Seaside] else []),
            (if guiProsperityChecked    gst then [Prosperity] else []),
            (if guiCornucopiaChecked    gst then [Cornucopia] else []),
            (if guiHinterlandsChecked   gst then [Hinterlands] else []),
            (if guiDarkAgesChecked      gst then [DarkAges] else []),
            (if guiGuildsChecked        gst then [Guilds] else []),
            (if guiEnvoyChecked         gst then [EnvoyPromo] else []),
            (if guiBlackMarketChecked   gst then [BlackMarketPromo] else []),
            (if guiGovernorChecked      gst then [GovernorPromo] else []),
            (if guiStashChecked         gst then [StashPromo] else []),
            (if guiWalledVillageChecked gst then [WalledVillagePromo] else []),
            (if guiCustomChecked        gst then [Custom] else [])
            ]
        emphasis = if not $ guiEmphasisChecked gst
                       then NoEmphasis
                       else case guiEmphasisValue gst of
                           0 -> RandomEmphasis
                           1 -> DominionEmphasis
                           2 -> IntrigueEmphasis
                           3 -> SeasideEmphasis
                           4 -> AlchemyEmphasis
                           5 -> ProsperityEmphasis
                           6 -> CornucopiaEmphasis
                           7 -> HinterlandsEmphasis
                           8 -> DarkAgesEmphasis
                           9 -> GuildsEmphasis
        filters = concat [
            (if guiActionFilterChecked gst then [actionFilter] else []),
            (if guiComplexityFilterChecked gst
             then [complexityFilter $ convertComplexityFilterValue $ guiComplexityFilterValue gst]
             else [])
            ]
        rules = concat [
            (if guiReactionRuleChecked gst
             then [reactionRule $ convertReactionRuleValue $ guiReactionRuleValue gst]
             else []),
            (if guiTrasherRuleChecked gst
             then [trasherRule $ convertTrasherRuleValue $ guiTrasherRuleValue gst]
             else []),
            (if guiInteractivityRuleChecked gst
             then [interactivityRule 2]
             else []),
            (if guiCostVarietyRuleChecked gst
             then [costVarietyRule]
             else [])
            ]
        colony = if guiColonyAdditionChecked gst then RandomColony else NoColony
        platinum = if not $ guiPlatinumAdditionChecked gst
                   then NoPlatinum
                   else case guiPlatinumAdditionValue gst of
                       0 -> RandomPlatinum
                       1 -> PlatinumWithColony
        shelters = if not $ guiSheltersAdditionChecked gst
                   then NoShelters
                   else case guiSheltersAdditionValue gst of
                       0 -> SheltersWithDarkAges
                       1 -> RandomShelters
        convertComplexityFilterValue v = case v of
            0 -> LowComplexityOnly
            1 -> MediumComplexityOrLower
            2 -> HighComplexityOrLower
        convertReactionRuleValue v = case v of
            0 -> RequireMoat
            1 -> RequireBlocker
            2 -> RequireReaction
        convertTrasherRuleValue v = case v of
            0 -> TrasherWithCurse
            1 -> AlwaysTrasher

main :: IO ()
main = do
    -- GUI initializations
    initGUI
    builder <- builderNew
    gladeFilepath <- getDataFileName "res/gui/gui.glade"
    builderAddFromFile builder gladeFilepath
    window <- builderGetObject builder castToWindow "dsgenWindow"
    on window deleteEvent (liftIO mainQuit >> return False)

    -- Get widgets
    selectedCardsTreeView  <- builderGetObject builder castToTreeView "cardTreeView"
    selectedCardsTreeModel <- liftM (either (\x -> error "No selectedCardsTreeView model")
                                            id)
                                    (runErrorT $ getTreeModel selectedCardsTreeView)

    -- Load cards
    cardFiles <- cardFileNames
    cardse <- runErrorT $ readCardFiles cardFiles
    case cardse of
        Left s -> startupErrorQuit $ show s
        Right cs -> do
            -- Hook up signals
            selectSetButton <- builderGetObject builder castToButton "selectSetButton"
            on selectSetButton buttonActivated $ fillSelection builder selectedCardsTreeModel cs

            widgetShowAll window
            mainGUI

fillSelection :: TreeModelClass self => Builder -> self -> [Card] -> IO ()
fillSelection b tm cs = do
    listStoreAppend tm "woo" -- TODO
{-
    runErrorT $ do
        gst <- readGUIState b
        let ssos = toSetSelectOptions gst
        return $ displayCardOutput tv $ setKingdomCards $ selectSet ssos cs
    case sgre of
        Left e -> displayOutput tv e
        Right sgr -> displayCardOutput tv $ setKingdomCards sgr

-- | Displays text in a TextView.
displayOutput :: TextView -> String -> IO ()
displayOutput tv s = do
    buf <- textViewGetBuffer tv
    textBufferSetText buf s
-}

{- | Displays an error messagebox and then immediately ends the program once
the box has been dismissed -}
startupErrorQuit :: String -> IO ()
startupErrorQuit s = do
    dialog <- messageDialogNew Nothing [] MessageError ButtonsOk s
    on dialog response (\rid -> liftIO mainQuit)
    widgetShowAll dialog
    mainGUI

-- | Displays an error message in a modal messagebox, as a child to a window.
displayError :: Window -> String -> IO ()
displayError w s = do
    dialog <- messageDialogNew (Just w)
                               [DialogModal, DialogDestroyWithParent]
                               MessageError
                               ButtonsOk
                               s
    dialogRun dialog
    widgetDestroy dialog
    return ()

{- | Returns a list of filepaths to all predefined card files. This is in the
'IO' monad since it uses cabal's "getDataFileName" function, which maps source
paths to their corresponding packaged location. -}
cardFileNames :: IO [String]
cardFileNames = mapM getPath files
  where getPath s = getDataFileName $ "res/cards/" ++ s ++ ".txt"
        files = ["dominion", "intrigue", "seaside", "alchemy", "prosperity",
                 "cornucopia", "hinterlands", "darkAges", "guilds", "promos",
                 "custom"]

