import Control.Monad.Error
import Control.Monad.Trans(liftIO)
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

    -- Widgets
    guiOutputTextView :: TextView
    }

-- | Read all state into a GUIState object
readGUIState :: Builder -> IO GUIState
readGUIState builder = do
    -- Sources
    dominionCheckButton      <- builderGetObject builder castToCheckButton "dominionCheckButton"
    dominionChecked          <- toggleButtonGetActive dominionCheckButton
    intrigueCheckButton      <- builderGetObject builder castToCheckButton "intrigueCheckButton"
    intrigueChecked          <- toggleButtonGetActive intrigueCheckButton
    seasideCheckButton       <- builderGetObject builder castToCheckButton "seasideCheckButton"
    seasideChecked           <- toggleButtonGetActive seasideCheckButton
    alchemyCheckButton       <- builderGetObject builder castToCheckButton "alchemyCheckButton"
    alchemyChecked           <- toggleButtonGetActive alchemyCheckButton
    prosperityCheckButton    <- builderGetObject builder castToCheckButton "prosperityCheckButton"
    prosperityChecked        <- toggleButtonGetActive prosperityCheckButton
    cornucopiaCheckButton    <- builderGetObject builder castToCheckButton "cornucopiaCheckButton"
    cornucopiaChecked        <- toggleButtonGetActive cornucopiaCheckButton
    hinterlandsCheckButton   <- builderGetObject builder castToCheckButton "hinterlandsCheckButton"
    hinterlandsChecked       <- toggleButtonGetActive hinterlandsCheckButton
    darkAgesCheckButton      <- builderGetObject builder castToCheckButton "darkAgesCheckButton"
    darkAgesChecked          <- toggleButtonGetActive darkAgesCheckButton
    guildsCheckButton        <- builderGetObject builder castToCheckButton "guildsCheckButton"
    guildsChecked            <- toggleButtonGetActive guildsCheckButton
    envoyCheckButton         <- builderGetObject builder castToCheckButton "envoyCheckButton"
    envoyChecked             <- toggleButtonGetActive envoyCheckButton
    blackMarketCheckButton   <- builderGetObject builder castToCheckButton "blackMarketCheckButton"
    blackMarketChecked       <- toggleButtonGetActive blackMarketCheckButton
    governorCheckButton      <- builderGetObject builder castToCheckButton "governorCheckButton"
    governorChecked          <- toggleButtonGetActive governorCheckButton
    stashCheckButton         <- builderGetObject builder castToCheckButton "stashCheckButton"
    stashChecked             <- toggleButtonGetActive stashCheckButton
    walledVillageCheckButton <- builderGetObject builder castToCheckButton "walledVillageCheckButton"
    walledVillageChecked     <- toggleButtonGetActive walledVillageCheckButton
    customCheckButton        <- builderGetObject builder castToCheckButton "customCheckButton"
    customChecked            <- toggleButtonGetActive customCheckButton

    -- Emphasis
    emphasisCheckButton <- builderGetObject builder castToCheckButton "emphasisCheckButton"
    emphasisChecked     <- toggleButtonGetActive emphasisCheckButton
    emphasisComboBox    <- builderGetObject builder castToComboBox "emphasisComboBox"
    emphasisValue       <- comboBoxGetActive emphasisComboBox

    -- Filters
    actionFilterCheckButton     <- builderGetObject builder castToCheckButton "actionFilterCheckButton"
    actionFilterChecked         <- toggleButtonGetActive actionFilterCheckButton
    complexityFilterCheckButton <- builderGetObject builder castToCheckButton "complexityFilterCheckButton"
    complexityFilterChecked     <- toggleButtonGetActive complexityFilterCheckButton
    complexityFilterComboBox    <- builderGetObject builder castToComboBox "complexityFilterComboBox"
    complexityFilterValue       <- comboBoxGetActive complexityFilterComboBox

    -- Rules
    costVarietyRuleCheckButton   <- builderGetObject builder castToCheckButton "costVarietyRuleCheckButton"
    costVarietyRuleChecked       <- toggleButtonGetActive costVarietyRuleCheckButton
    interactivityRuleCheckButton <- builderGetObject builder castToCheckButton "interactivityRuleCheckButton"
    interactivityRuleChecked     <- toggleButtonGetActive interactivityRuleCheckButton
    reactionRuleCheckButton      <- builderGetObject builder castToCheckButton "reactionRuleCheckButton"
    reactionRuleChecked          <- toggleButtonGetActive reactionRuleCheckButton
    reactionRuleComboBox         <- builderGetObject builder castToComboBox "reactionRuleComboBox"
    reactionRuleValue            <- comboBoxGetActive reactionRuleComboBox
    trasherRuleCheckButton       <- builderGetObject builder castToCheckButton "trasherRuleCheckButton"
    trasherRuleChecked           <- toggleButtonGetActive trasherRuleCheckButton
    trasherRuleComboBox          <- builderGetObject builder castToComboBox "trasherRuleComboBox"
    trasherRuleValue             <- comboBoxGetActive trasherRuleComboBox

    -- Additions
    colonyAdditionCheckButton   <- builderGetObject builder castToCheckButton "colonyAdditionCheckButton"
    colonyAdditionChecked       <- toggleButtonGetActive colonyAdditionCheckButton
    platinumAdditionCheckButton <- builderGetObject builder castToCheckButton "platinumAdditionCheckButton"
    platinumAdditionChecked     <- toggleButtonGetActive platinumAdditionCheckButton
    platinumAdditionComboBox    <- builderGetObject builder castToComboBox "platinumAdditionComboBox"
    platinumAdditionValue       <- comboBoxGetActive platinumAdditionComboBox
    sheltersAdditionCheckButton <- builderGetObject builder castToCheckButton "sheltersAdditionCheckButton"
    sheltersAdditionChecked     <- toggleButtonGetActive sheltersAdditionCheckButton
    sheltersAdditionComboBox    <- builderGetObject builder castToComboBox "sheltersAdditionComboBox"
    sheltersAdditionValue       <- comboBoxGetActive sheltersAdditionComboBox

    -- Widgets
    outputTextView <- builderGetObject builder castToTextView "outputTextView"

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

        -- Widgets
        guiOutputTextView = outputTextView
        }

instance SetOptionable GUIState where
    toSetSelectOptions gst = SetSelectOptions {
        setSelectSources = sources,
        setSelectEmphasis = emphasis,
        setSelectFilters = filters,
        setSelectRules = rules,
        setSelectColonyAddition = colony,
        setSelectPlatinumAddition = platinum,
        setSelectSheltersAddition = shelters
        }
      where sources = concat [
                (if guiDominionChecked gst then [Dominion] else []),
                (if guiIntrigueChecked gst then [Intrigue] else []),
                (if guiAlchemyChecked gst then [Alchemy] else []),
                (if guiSeasideChecked gst then [Seaside] else []),
                (if guiProsperityChecked gst then [Prosperity] else []),
                (if guiCornucopiaChecked gst then [Cornucopia] else []),
                (if guiHinterlandsChecked gst then [Hinterlands] else []),
                (if guiDarkAgesChecked gst then [DarkAges] else []),
                (if guiGuildsChecked gst then [Guilds] else []),
                (if guiEnvoyChecked gst then [EnvoyPromo] else []),
                (if guiBlackMarketChecked gst then [BlackMarketPromo] else []),
                (if guiGovernorChecked gst then [GovernorPromo] else []),
                (if guiStashChecked gst then [StashPromo] else []),
                (if guiWalledVillageChecked gst then [WalledVillagePromo] else []),
                (if guiCustomChecked gst then [Custom] else [])
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
                0 -> LowComplexityRuleOption
                1 -> MediumComplexityRuleOption
                2 -> HighComplexityRuleOption
            convertReactionRuleValue v = case v of
                0 -> MoatReaction
                1 -> BlockerReaction
                2 -> ReactionReaction
            convertTrasherRuleValue v = case v of
                0 -> CurseTrasher
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

    -- Load cards, displaying popup and quitting if this fails
    cardfiles <- cardFileNames
    cardse <- runErrorT $ readCardFiles cardFiles
    case cardse of
        Left s -> startupErrorQuit $ show s
        Right cs -> do
            -- Hook up signals
            outputTextView <- builderGetObject builder castToTextView "outputTextView"

            selectSetButton <- builderGetObject builder castToButton "selectSetButton"
            on selectSetButton buttonActivated $ selectAndDisplaySet builder window outputTextView cs

            widgetShowAll window
            mainGUI

selectAndDisplaySet :: Builder -> Window -> TextView -> [Card] -> IO ()
selectAndDisplaySet b w tv cs = do
    gst <- readGUIState b
    let ssos = toSetSelectOptions gst
    sgre <- selectSet ssos cs
    case sgre of
        Left e -> displayError w e
        Right sgr -> displayCardOutput tv $ setKingdomCards sgr

-- | Displays a list of prettyf-formatted card names in a TextView.
displayCardOutput :: TextView -> [Card] -> IO ()
displayCardOutput tv cs = displayOutput tv $ intercalate ", " $ sort $ map showCardName cs

-- | Displays text in a TextView.
displayOutput :: TextView -> String -> IO ()
displayOutput tv s = do
    buf <- textViewGetBuffer tv
    textBufferSetText buf s

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
    return ()

{- | Returns a list of filepaths to all predefined card files. This is in the
'IO' monad since it uses cabal's "getDataFileName" function, which maps source
paths to their corresponding packaged location. -}
cardFileNames :: IO [String]
cardFileNames = mapM getPath files
  where getPath s = getDataFileName $ "res/cards/" ++ s ++ ".txt"
        files = ["dominion", "intrigue", "seaside", "alchemy", "prosperity",
                 "cornucopia", "hinterlands", "darkAges", "guilds", "custom"]

