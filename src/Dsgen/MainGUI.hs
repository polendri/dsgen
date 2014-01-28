import Control.Monad.Error
import Control.Monad.Trans(liftIO)
import qualified Data.Foldable as F
import Data.List
import Graphics.UI.Gtk
import System.Random

import Paths_dsgen
import Dsgen.Cards
import Dsgen.SetSelect

{- | Holds the state of all GUI widgets which we might be interested in
reading, and the actual widgets we might be interested in modifying -}
data GUIState  = GUIState {
    -- Sources
    guiDominionChecked      :: Bool,
    guiIntrigueChecked      :: Bool,
    guiSeasideChecked       :: Bool,
    guiAlchemyChecked       :: Bool,
    guiProsperityChecked    :: Bool,
    guiCornucopiaChecked    :: Bool,
    guiHinterlandsChecked   :: Bool,
    guiDarkAgesChecked      :: Bool,
    guiGuildsChecked        :: Bool,
    guiEnvoyChecked         :: Bool,
    guiBlackMarketChecked   :: Bool,
    guiGovernorChecked      :: Bool,
    guiStashChecked         :: Bool,
    guiWalledVillageChecked :: Bool,
    guiCustomChecked        :: Bool,

    -- Emphasis
    guiEmphasisChecked  :: Bool,
    guiEmphasisValue    :: Int,

    -- Filters
    guiActionFilterChecked     :: Bool,
    guiComplexityFilterChecked :: Bool,
    guiComplexityFilterValue   :: Int,

    -- Rules
    guiCostVarietyRuleChecked   :: Bool,
    guiInteractivityRuleChecked :: Bool,
    guiReactionRuleChecked      :: Bool,
    guiReactionRuleValue        :: Int,
    guiTrasherRuleChecked       :: Bool,
    guiTrasherRuleValue         :: Int,

    -- Additions
    guiColonyAdditionChecked    :: Bool,
    guiPlatinumAdditionChecked  :: Bool,
    guiPlatinumAdditionValue    :: Int,
    guiSheltersAdditionChecked  :: Bool,
    guiSheltersAdditionValue    :: Int,

    -- Card lists
    guiSelectedCards       :: [Card],
    guiManuallyPickedCards :: [Card],

    -- Widgets
    guiOutputTextView :: TextView
    }

-- | Read all state into a GUIState object
readGUIState :: Builder -> [Card] -> ErrorT String IO GUIState
readGUIState builder cs = do
    -- Sources
    dominionCheckButton      <- liftIO $ builderGetObject builder castToCheckButton "dominionCheckButton"
    dominionChecked          <- liftIO $ toggleButtonGetActive dominionCheckButton
    intrigueCheckButton      <- liftIO $ builderGetObject builder castToCheckButton "intrigueCheckButton"
    intrigueChecked          <- liftIO $ toggleButtonGetActive intrigueCheckButton
    seasideCheckButton       <- liftIO $ builderGetObject builder castToCheckButton "seasideCheckButton"
    seasideChecked           <- liftIO $ toggleButtonGetActive seasideCheckButton
    alchemyCheckButton       <- liftIO $ builderGetObject builder castToCheckButton "alchemyCheckButton"
    alchemyChecked           <- liftIO $ toggleButtonGetActive alchemyCheckButton
    prosperityCheckButton    <- liftIO $ builderGetObject builder castToCheckButton "prosperityCheckButton"
    prosperityChecked        <- liftIO $ toggleButtonGetActive prosperityCheckButton
    cornucopiaCheckButton    <- liftIO $ builderGetObject builder castToCheckButton "cornucopiaCheckButton"
    cornucopiaChecked        <- liftIO $ toggleButtonGetActive cornucopiaCheckButton
    hinterlandsCheckButton   <- liftIO $ builderGetObject builder castToCheckButton "hinterlandsCheckButton"
    hinterlandsChecked       <- liftIO $ toggleButtonGetActive hinterlandsCheckButton
    darkAgesCheckButton      <- liftIO $ builderGetObject builder castToCheckButton "darkAgesCheckButton"
    darkAgesChecked          <- liftIO $ toggleButtonGetActive darkAgesCheckButton
    guildsCheckButton        <- liftIO $ builderGetObject builder castToCheckButton "guildsCheckButton"
    guildsChecked            <- liftIO $ toggleButtonGetActive guildsCheckButton
    envoyCheckButton         <- liftIO $ builderGetObject builder castToCheckButton "envoyCheckButton"
    envoyChecked             <- liftIO $ toggleButtonGetActive envoyCheckButton
    blackMarketCheckButton   <- liftIO $ builderGetObject builder castToCheckButton "blackMarketCheckButton"
    blackMarketChecked       <- liftIO $ toggleButtonGetActive blackMarketCheckButton
    governorCheckButton      <- liftIO $ builderGetObject builder castToCheckButton "governorCheckButton"
    governorChecked          <- liftIO $ toggleButtonGetActive governorCheckButton
    stashCheckButton         <- liftIO $ builderGetObject builder castToCheckButton "stashCheckButton"
    stashChecked             <- liftIO $ toggleButtonGetActive stashCheckButton
    walledVillageCheckButton <- liftIO $ builderGetObject builder castToCheckButton "walledVillageCheckButton"
    walledVillageChecked     <- liftIO $ toggleButtonGetActive walledVillageCheckButton
    customCheckButton        <- liftIO $ builderGetObject builder castToCheckButton "customCheckButton"
    customChecked            <- liftIO $ toggleButtonGetActive customCheckButton

    -- Emphasis
    emphasisCheckButton <- liftIO $ builderGetObject builder castToCheckButton "emphasisCheckButton"
    emphasisChecked     <- liftIO $ toggleButtonGetActive emphasisCheckButton
    emphasisComboBox    <- liftIO $ builderGetObject builder castToComboBox "emphasisComboBox"
    emphasisValue       <- liftIO $ comboBoxGetActive emphasisComboBox

    -- Filters
    actionFilterCheckButton     <- liftIO $ builderGetObject builder castToCheckButton "actionFilterCheckButton"
    actionFilterChecked         <- liftIO $ toggleButtonGetActive actionFilterCheckButton
    complexityFilterCheckButton <- liftIO $ builderGetObject builder castToCheckButton "complexityFilterCheckButton"
    complexityFilterChecked     <- liftIO $ toggleButtonGetActive complexityFilterCheckButton
    complexityFilterComboBox    <- liftIO $ builderGetObject builder castToComboBox "complexityFilterComboBox"
    complexityFilterValue       <- liftIO $ comboBoxGetActive complexityFilterComboBox

    -- Rules
    costVarietyRuleCheckButton   <- liftIO $ builderGetObject builder castToCheckButton "costVarietyRuleCheckButton"
    costVarietyRuleChecked       <- liftIO $ toggleButtonGetActive costVarietyRuleCheckButton
    interactivityRuleCheckButton <- liftIO $ builderGetObject builder castToCheckButton "interactivityRuleCheckButton"
    interactivityRuleChecked     <- liftIO $ toggleButtonGetActive interactivityRuleCheckButton
    reactionRuleCheckButton      <- liftIO $ builderGetObject builder castToCheckButton "reactionRuleCheckButton"
    reactionRuleChecked          <- liftIO $ toggleButtonGetActive reactionRuleCheckButton
    reactionRuleComboBox         <- liftIO $ builderGetObject builder castToComboBox "reactionRuleComboBox"
    reactionRuleValue            <- liftIO $ comboBoxGetActive reactionRuleComboBox
    trasherRuleCheckButton       <- liftIO $ builderGetObject builder castToCheckButton "trasherRuleCheckButton"
    trasherRuleChecked           <- liftIO $ toggleButtonGetActive trasherRuleCheckButton
    trasherRuleComboBox          <- liftIO $ builderGetObject builder castToComboBox "trasherRuleComboBox"
    trasherRuleValue             <- liftIO $ comboBoxGetActive trasherRuleComboBox

    -- Additions
    colonyAdditionCheckButton   <- liftIO $ builderGetObject builder castToCheckButton "colonyAdditionCheckButton"
    colonyAdditionChecked       <- liftIO $ toggleButtonGetActive colonyAdditionCheckButton
    platinumAdditionCheckButton <- liftIO $ builderGetObject builder castToCheckButton "platinumAdditionCheckButton"
    platinumAdditionChecked     <- liftIO $ toggleButtonGetActive platinumAdditionCheckButton
    platinumAdditionComboBox    <- liftIO $ builderGetObject builder castToComboBox "platinumAdditionComboBox"
    platinumAdditionValue       <- liftIO $ comboBoxGetActive platinumAdditionComboBox
    sheltersAdditionCheckButton <- liftIO $ builderGetObject builder castToCheckButton "sheltersAdditionCheckButton"
    sheltersAdditionChecked     <- liftIO $ toggleButtonGetActive sheltersAdditionCheckButton
    sheltersAdditionComboBox    <- liftIO $ builderGetObject builder castToComboBox "sheltersAdditionComboBox"
    sheltersAdditionValue       <- liftIO $ comboBoxGetActive sheltersAdditionComboBox

    -- Card lists
    selectedCardsTreeView        <- liftIO $ builderGetObject builder castToTreeView "cardTreeView"
    selectedCardsTreeModel       <- getTreeModel selectedCardsTreeView
    selectedCards                <- liftIO $ liftM (getCardsByName cs) (getAllCardNames selectedCardsTreeModel)
    manuallyPickedCardsTreeView  <- liftIO $ builderGetObject builder castToTreeView "pickedCardTreeView"
    manuallyPickedCardsTreeModel <- getTreeModel manuallyPickedCardsTreeView
    manuallyPickedCards          <- liftIO $ liftM (getCardsByName cs) (getAllCardNames manuallyPickedCardsTreeModel)

    -- Widgets
    outputTextView <- liftIO $ builderGetObject builder castToTextView "outputTextView"

    return GUIState {
        -- Sources
        guiDominionChecked      = dominionChecked,
        guiIntrigueChecked      = intrigueChecked,
        guiSeasideChecked       = seasideChecked,
        guiAlchemyChecked       = alchemyChecked,
        guiProsperityChecked    = prosperityChecked,
        guiCornucopiaChecked    = cornucopiaChecked,
        guiHinterlandsChecked   = hinterlandsChecked,
        guiDarkAgesChecked      = darkAgesChecked,
        guiGuildsChecked        = guildsChecked,
        guiEnvoyChecked         = envoyChecked,
        guiBlackMarketChecked   = blackMarketChecked,
        guiGovernorChecked      = governorChecked,
        guiStashChecked         = stashChecked,
        guiWalledVillageChecked = walledVillageChecked,
        guiCustomChecked        = customChecked,

        -- Emphasis
        guiEmphasisChecked  = emphasisChecked,
        guiEmphasisValue    = emphasisValue,

        -- Filters
        guiActionFilterChecked     = actionFilterChecked,
        guiComplexityFilterChecked = complexityFilterChecked,
        guiComplexityFilterValue   = complexityFilterValue,

        -- Rules
        guiCostVarietyRuleChecked   = costVarietyRuleChecked,
        guiInteractivityRuleChecked = interactivityRuleChecked,
        guiReactionRuleChecked      = reactionRuleChecked,
        guiReactionRuleValue        = reactionRuleValue,
        guiTrasherRuleChecked       = trasherRuleChecked,
        guiTrasherRuleValue         = trasherRuleValue,

        -- Additions
        guiColonyAdditionChecked    = colonyAdditionChecked,
        guiPlatinumAdditionChecked  = platinumAdditionChecked,
        guiPlatinumAdditionValue    = platinumAdditionValue,
        guiSheltersAdditionChecked  = sheltersAdditionChecked,
        guiSheltersAdditionValue    = sheltersAdditionValue,

        -- Card lists
        guiSelectedCards       = selectedCards,
        guiManuallyPickedCards = manuallyPickedCards,

        -- Widgets
        guiOutputTextView = outputTextView
        }

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

{- | Given a list of cards, and a list of card names, returns just the cards
with the names specified. -}
getCardsByName :: [Card] -> [String] -> [Card]
getCardsByName [] _ = []
getCardsByName (c:cs) ss = if elem (cardName c) ss then c : rest else rest
  where rest = getCardsByName cs ss

-- | Gets all strings in the first column of a 'TreeModel'.
getAllCardNames :: TreeModelClass self => self -> IO [String]
getAllCardNames tm = getAllValues tm =<< (treeModelGetIterFirst tm)
  where getAllValues tm iterm =
            case iterm of
                Nothing -> return []
                Just iter -> do
                    val <- treeModelGetValue tm iter (makeColumnIdString 0)
                    iterm' <- treeModelIterNext tm iter
                    liftM (val:) (getAllValues tm iterm')

-- | Gets a 'TreeModel' from a 'TreeView', throwing an error if no model exists.
getTreeModel :: TreeViewClass self => self -> ErrorT String IO TreeModel
getTreeModel tv = do
    tmm <- liftIO $ treeViewGetModel tv
    case tmm of
        Nothing -> throwError "No model defined for TreeView"
        Just tm -> return tm

