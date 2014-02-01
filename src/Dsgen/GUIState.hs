module Dsgen.GUIState (
    GUI(..),
    GUIState(..),
    CardListRow(..),
    readGUI,
    getGUIState,
    mkSetSelectOptions,
    cardListAppend,
    cardListRemove
    ) where

import Control.Monad.Error
import Control.Monad.Trans(liftIO)
import Data.List((\\), sort)
import Graphics.UI.Gtk

import Dsgen.Cards
import Dsgen.SetSelect


-- | Holds all the relevant widgets from the UI.
data GUI = GUI {
    -- Sources
    dominionCheckButton      :: CheckButton,
    intrigueCheckButton      :: CheckButton,
    seasideCheckButton       :: CheckButton,
    alchemyCheckButton       :: CheckButton,
    prosperityCheckButton    :: CheckButton,
    cornucopiaCheckButton    :: CheckButton,
    hinterlandsCheckButton   :: CheckButton,
    darkAgesCheckButton      :: CheckButton,
    guildsCheckButton        :: CheckButton,
    envoyCheckButton         :: CheckButton,
    blackMarketCheckButton   :: CheckButton,
    governorCheckButton      :: CheckButton,
    stashCheckButton         :: CheckButton,
    walledVillageCheckButton :: CheckButton,
    customCheckButton        :: CheckButton,

    -- Emphasis
    emphasisCheckButton :: CheckButton,
    emphasisComboBox    :: ComboBox,

    -- Filters
    actionFilterCheckButton     :: CheckButton,
    complexityFilterCheckButton :: CheckButton,
    complexityFilterComboBox    :: ComboBox,

    -- Rules
    costVarietyRuleCheckButton   :: CheckButton,
    interactivityRuleCheckButton :: CheckButton,
    interactivityRuleComboBox    :: ComboBox,
    reactionRuleCheckButton      :: CheckButton,
    reactionRuleComboBox         :: ComboBox,
    trasherRuleCheckButton       :: CheckButton,
    trasherRuleComboBox          :: ComboBox,

    -- Additions
    colPlatAdditionCheckButton  :: CheckButton,
    sheltersAdditionCheckButton :: CheckButton,
    sheltersAdditionComboBox    :: ComboBox,

    -- Card lists and other output widgets
    randomCardsTreeView  :: TreeView,
    randomCardsListStore :: ListStore CardListRow,
    manualCardsTreeView  :: TreeView,
    manualCardsListStore :: ListStore CardListRow,
    vetoedCardsTreeView  :: TreeView,
    vetoedCardsListStore :: ListStore CardListRow,
    colPlatLabel         :: Label,
    sheltersLabel        :: Label,

    -- Buttons
    addManualCardButton  :: Button,
    rmvManualCardsButton :: Button,
    addVetoedCardButton  :: Button,
    rmvVetoedCardsButton :: Button,
    selectSetButton      :: Button,

    -- Containers
    mainNotebook :: Notebook
    }

{- | Holds the state of all GUI widgets which we might be interested in
reading, and the actual widgets we might be interested in modifying -}
data GUIState = GUIState {
    -- Sources
    dominionCheckedState      :: Bool,
    intrigueCheckedState      :: Bool,
    seasideCheckedState       :: Bool,
    alchemyCheckedState       :: Bool,
    prosperityCheckedState    :: Bool,
    cornucopiaCheckedState    :: Bool,
    hinterlandsCheckedState   :: Bool,
    darkAgesCheckedState      :: Bool,
    guildsCheckedState        :: Bool,
    envoyCheckedState         :: Bool,
    blackMarketCheckedState   :: Bool,
    governorCheckedState      :: Bool,
    stashCheckedState         :: Bool,
    walledVillageCheckedState :: Bool,
    customCheckedState        :: Bool,

    -- Emphasis
    emphasisCheckedState :: Bool,
    emphasisValue        :: Int,

    -- Filters
    actionFilterCheckedState     :: Bool,
    complexityFilterCheckedState :: Bool,
    complexityFilterValue        :: Int,

    -- Rules
    costVarietyRuleCheckedState   :: Bool,
    interactivityRuleCheckedState :: Bool,
    interactivityRuleValue        :: Int,
    reactionRuleCheckedState      :: Bool,
    reactionRuleValue             :: Int,
    trasherRuleCheckedState       :: Bool,
    trasherRuleValue              :: Int,

    -- Additions
    colPlatAdditionCheckedState  :: Bool,
    sheltersAdditionCheckedState :: Bool,
    sheltersAdditionValue        :: Int,

    -- Card lists
    randomCardsList :: [Card],
    manualCardsList :: [Card],
    vetoedCardsList :: [Card]
    } deriving (Show)

data CardListRow = CardListRow {
    col1 :: String,
    col2 :: String,
    col3 :: String
    } deriving (Read, Show)

-- | Create a 'GUI' object based on the contents of a Glade Builder.
readGUI :: Builder -> IO GUI
readGUI b = do
    -- Sources
    dominionCB      <- liftIO $ builderGetObject b castToCheckButton "dominionCheckButton"
    intrigueCB      <- liftIO $ builderGetObject b castToCheckButton "intrigueCheckButton"
    seasideCB       <- liftIO $ builderGetObject b castToCheckButton "seasideCheckButton"
    alchemyCB       <- liftIO $ builderGetObject b castToCheckButton "alchemyCheckButton"
    prosperityCB    <- liftIO $ builderGetObject b castToCheckButton "prosperityCheckButton"
    cornucopiaCB    <- liftIO $ builderGetObject b castToCheckButton "cornucopiaCheckButton"
    hinterlandsCB   <- liftIO $ builderGetObject b castToCheckButton "hinterlandsCheckButton"
    darkAgesCB      <- liftIO $ builderGetObject b castToCheckButton "darkAgesCheckButton"
    guildsCB        <- liftIO $ builderGetObject b castToCheckButton "guildsCheckButton"
    envoyCB         <- liftIO $ builderGetObject b castToCheckButton "envoyCheckButton"
    blackMarketCB   <- liftIO $ builderGetObject b castToCheckButton "blackMarketCheckButton"
    governorCB      <- liftIO $ builderGetObject b castToCheckButton "governorCheckButton"
    stashCB         <- liftIO $ builderGetObject b castToCheckButton "stashCheckButton"
    walledVillageCB <- liftIO $ builderGetObject b castToCheckButton "walledVillageCheckButton"
    customCB        <- liftIO $ builderGetObject b castToCheckButton "customCheckButton"

    -- Emphasis
    emphasisCB  <- liftIO $ builderGetObject b castToCheckButton "emphasisCheckButton"
    emphasisCBX <- liftIO $ builderGetObject b castToComboBox    "emphasisComboBox"

    -- Filters
    actionFilterCB      <- liftIO $ builderGetObject b castToCheckButton "actionFilterCheckButton"
    complexityFilterCB  <- liftIO $ builderGetObject b castToCheckButton "complexityFilterCheckButton"
    complexityFilterCBX <- liftIO $ builderGetObject b castToComboBox    "complexityFilterComboBox"

    -- Rules
    costVarietyRuleCB    <- liftIO $ builderGetObject b castToCheckButton "costVarietyRuleCheckButton"
    interactivityRuleCB  <- liftIO $ builderGetObject b castToCheckButton "interactivityRuleCheckButton"
    interactivityRuleCBX <- liftIO $ builderGetObject b castToComboBox    "interactivityRuleComboBox"
    reactionRuleCB       <- liftIO $ builderGetObject b castToCheckButton "reactionRuleCheckButton"
    reactionRuleCBX      <- liftIO $ builderGetObject b castToComboBox    "reactionRuleComboBox"
    trasherRuleCB        <- liftIO $ builderGetObject b castToCheckButton "trasherRuleCheckButton"
    trasherRuleCBX       <- liftIO $ builderGetObject b castToComboBox    "trasherRuleComboBox"

    -- Addition s
    colPlatAdditionCB    <- liftIO $ builderGetObject b castToCheckButton "colPlatAdditionCheckButton"
    sheltersAdditionCB   <- liftIO $ builderGetObject b castToCheckButton "sheltersAdditionCheckButton"
    sheltersAdditionCBX  <- liftIO $ builderGetObject b castToComboBox    "sheltersAdditionComboBox"

    -- Card lists and other output widgets
    randomCardsTV <- liftIO $ builderGetObject b castToTreeView "randomCardsTreeView"
    randomCardsLS <- liftIO $ cardListCreate randomCardsTV
    manualCardsTV <- liftIO $ builderGetObject b castToTreeView "manualCardsTreeView"
    manualCardsLS <- liftIO $ cardListCreate manualCardsTV
    vetoedCardsTV <- liftIO $ builderGetObject b castToTreeView "vetoedCardsTreeView"
    vetoedCardsLS <- liftIO $ cardListCreate vetoedCardsTV
    colPlatL      <- liftIO $ builderGetObject b castToLabel "colPlatLabel"
    sheltersL     <- liftIO $ builderGetObject b castToLabel "sheltersLabel"

    -- Buttons
    addManualCardB  <- liftIO $ builderGetObject b castToButton "addManualCardButton"
    rmvManualCardsB <- liftIO $ builderGetObject b castToButton "rmvManualCardsButton"
    addVetoedCardB  <- liftIO $ builderGetObject b castToButton "addVetoedCardButton"
    rmvVetoedCardsB <- liftIO $ builderGetObject b castToButton "rmvVetoedCardsButton"
    selectSetB      <- liftIO $ builderGetObject b castToButton "selectSetButton"

    -- Containers
    mainNB <- liftIO $ builderGetObject b castToNotebook "mainNotebook"

    return $ GUI {
        -- Sources
        dominionCheckButton      = dominionCB,
        intrigueCheckButton      = intrigueCB,
        seasideCheckButton       = seasideCB,
        alchemyCheckButton       = alchemyCB,
        prosperityCheckButton    = prosperityCB,
        cornucopiaCheckButton    = cornucopiaCB,
        hinterlandsCheckButton   = hinterlandsCB,
        darkAgesCheckButton      = darkAgesCB,
        guildsCheckButton        = guildsCB,
        envoyCheckButton         = envoyCB,
        blackMarketCheckButton   = blackMarketCB,
        governorCheckButton      = governorCB,
        stashCheckButton         = stashCB,
        walledVillageCheckButton = walledVillageCB,
        customCheckButton        = customCB,

        -- Emphasis
        emphasisCheckButton = emphasisCB,
        emphasisComboBox    = emphasisCBX,

        -- Filters
        actionFilterCheckButton     = actionFilterCB,
        complexityFilterCheckButton = complexityFilterCB,
        complexityFilterComboBox    = complexityFilterCBX,

        -- Rules
        costVarietyRuleCheckButton   = costVarietyRuleCB,
        interactivityRuleCheckButton = interactivityRuleCB,
        interactivityRuleComboBox    = interactivityRuleCBX,
        reactionRuleCheckButton      = reactionRuleCB,
        reactionRuleComboBox         = reactionRuleCBX,
        trasherRuleCheckButton       = trasherRuleCB,
        trasherRuleComboBox          = trasherRuleCBX,

        -- Additions
        colPlatAdditionCheckButton  = colPlatAdditionCB,
        sheltersAdditionCheckButton = sheltersAdditionCB,
        sheltersAdditionComboBox    = sheltersAdditionCBX,

        -- Card lists and other output widgets
        randomCardsTreeView  = randomCardsTV,
        randomCardsListStore = randomCardsLS,
        manualCardsTreeView  = manualCardsTV,
        manualCardsListStore = manualCardsLS,
        vetoedCardsTreeView  = vetoedCardsTV,
        vetoedCardsListStore = vetoedCardsLS,
        colPlatLabel         = colPlatL,
        sheltersLabel        = sheltersL,

        -- Buttons
        addManualCardButton  = addManualCardB,
        rmvManualCardsButton = rmvManualCardsB,
        addVetoedCardButton  = addVetoedCardB,
        rmvVetoedCardsButton = rmvVetoedCardsB,
        selectSetButton      = selectSetB,

        -- Containers
        mainNotebook = mainNB
        }

-- | Create a 'GUIState' object based on the contents of a 'GUI' object.
getGUIState :: GUI -> [Card] -> IO GUIState
getGUIState gui cs = do
    -- Sources
    dominionCS      <- toggleButtonGetActive $ dominionCheckButton      gui
    intrigueCS      <- toggleButtonGetActive $ intrigueCheckButton      gui
    seasideCS       <- toggleButtonGetActive $ seasideCheckButton       gui
    alchemyCS       <- toggleButtonGetActive $ alchemyCheckButton       gui
    prosperityCS    <- toggleButtonGetActive $ prosperityCheckButton    gui
    cornucopiaCS    <- toggleButtonGetActive $ cornucopiaCheckButton    gui
    hinterlandsCS   <- toggleButtonGetActive $ hinterlandsCheckButton   gui
    darkAgesCS      <- toggleButtonGetActive $ darkAgesCheckButton      gui
    guildsCS        <- toggleButtonGetActive $ guildsCheckButton        gui
    envoyCS         <- toggleButtonGetActive $ envoyCheckButton         gui
    blackMarketCS   <- toggleButtonGetActive $ blackMarketCheckButton   gui
    governorCS      <- toggleButtonGetActive $ governorCheckButton      gui
    stashCS         <- toggleButtonGetActive $ stashCheckButton         gui
    walledVillageCS <- toggleButtonGetActive $ walledVillageCheckButton gui
    customCS        <- toggleButtonGetActive $ customCheckButton        gui

    -- Emphasis
    emphasisCS <- toggleButtonGetActive $ emphasisCheckButton gui
    emphasisV  <- comboBoxGetActive $ emphasisComboBox gui

    -- Filters
    actionFilterCS     <- toggleButtonGetActive $ actionFilterCheckButton     gui
    complexityFilterCS <- toggleButtonGetActive $ complexityFilterCheckButton gui
    complexityFilterV  <- comboBoxGetActive     $ complexityFilterComboBox    gui

    -- Rules
    costVarietyRuleCS   <- toggleButtonGetActive $ costVarietyRuleCheckButton   gui
    interactivityRuleCS <- toggleButtonGetActive $ interactivityRuleCheckButton gui
    interactivityRuleV  <- comboBoxGetActive     $ interactivityRuleComboBox    gui
    reactionRuleCS      <- toggleButtonGetActive $ reactionRuleCheckButton      gui
    reactionRuleV       <- comboBoxGetActive     $ reactionRuleComboBox         gui
    trasherRuleCS       <- toggleButtonGetActive $ trasherRuleCheckButton       gui
    trasherRuleV        <- comboBoxGetActive     $ trasherRuleComboBox          gui

    -- Additions
    colPlatAdditionCS  <- toggleButtonGetActive $ colPlatAdditionCheckButton  gui
    sheltersAdditionCS <- toggleButtonGetActive $ sheltersAdditionCheckButton gui
    sheltersAdditionV  <- comboBoxGetActive     $ sheltersAdditionComboBox    gui

    -- Card lists
    randomCardNames <- getAllCardNames $ randomCardsListStore gui
    let randomCL    = filter (\c -> elem (cardName c) randomCardNames) cs
    manualCardNames <- getAllCardNames $ manualCardsListStore gui
    let manualCL    = filter (\c -> elem (cardName c) manualCardNames) cs
    vetoedCardNames <- getAllCardNames $ vetoedCardsListStore gui
    let vetoedCL    = filter (\c -> elem (cardName c) vetoedCardNames) cs

    return $ GUIState {
        -- Sources
        dominionCheckedState      = dominionCS,
        intrigueCheckedState      = intrigueCS,
        seasideCheckedState       = seasideCS,
        alchemyCheckedState       = alchemyCS,
        prosperityCheckedState    = prosperityCS,
        cornucopiaCheckedState    = cornucopiaCS,
        hinterlandsCheckedState   = hinterlandsCS,
        darkAgesCheckedState      = darkAgesCS,
        guildsCheckedState        = guildsCS,
        envoyCheckedState         = envoyCS,
        blackMarketCheckedState   = blackMarketCS,
        governorCheckedState      = governorCS,
        stashCheckedState         = stashCS,
        walledVillageCheckedState = walledVillageCS,
        customCheckedState        = customCS,

        -- Emphasis
        emphasisCheckedState = emphasisCS,
        emphasisValue        = emphasisV,

        -- Filters
        actionFilterCheckedState     = actionFilterCS,
        complexityFilterCheckedState = complexityFilterCS,
        complexityFilterValue        = complexityFilterV,

        -- Rules
        costVarietyRuleCheckedState   = costVarietyRuleCS,
        interactivityRuleCheckedState = interactivityRuleCS,
        interactivityRuleValue        = interactivityRuleV,
        reactionRuleCheckedState      = reactionRuleCS,
        reactionRuleValue             = reactionRuleV,
        trasherRuleCheckedState       = trasherRuleCS,
        trasherRuleValue              = trasherRuleV,

        -- Additions
        colPlatAdditionCheckedState  = colPlatAdditionCS,
        sheltersAdditionCheckedState = sheltersAdditionCS,
        sheltersAdditionValue        = sheltersAdditionV,

        -- Card lists
        randomCardsList = randomCL,
        manualCardsList = manualCL,
        vetoedCardsList = vetoedCL
        }

-- | Builds a 'SetSelectOptions' object based on the GUI state and a card pool.
mkSetSelectOptions :: GUIState -> [Card] -> SetSelectOptions
mkSetSelectOptions gst cs = SetSelectOptions {
    ssoPickCount        = 10 - (length manualCards), -- TODO: Non-hardcode 10
    ssoPool             = (cs \\ manualCards) \\ (vetoedCardsList gst),
    ssoManualPicks      = manualCards,
    ssoSources          = sources,
    ssoEmphasis         = emphasis,
    ssoFilters          = filters,
    ssoRules            = rules,
    ssoColPlatAddition  = colPlat,
    ssoSheltersAddition = shelters
    }
  where manualCards = manualCardsList gst
        sources = concat [
            (if dominionCheckedState      gst then [Dominion]           else []),
            (if intrigueCheckedState      gst then [Intrigue]           else []),
            (if alchemyCheckedState       gst then [Alchemy]            else []),
            (if seasideCheckedState       gst then [Seaside]            else []),
            (if prosperityCheckedState    gst then [Prosperity]         else []),
            (if cornucopiaCheckedState    gst then [Cornucopia]         else []),
            (if hinterlandsCheckedState   gst then [Hinterlands]        else []),
            (if darkAgesCheckedState      gst then [DarkAges]           else []),
            (if guildsCheckedState        gst then [Guilds]             else []),
            (if envoyCheckedState         gst then [EnvoyPromo]         else []),
            (if blackMarketCheckedState   gst then [BlackMarketPromo]   else []),
            (if governorCheckedState      gst then [GovernorPromo]      else []),
            (if stashCheckedState         gst then [StashPromo]         else []),
            (if walledVillageCheckedState gst then [WalledVillagePromo] else []),
            (if customCheckedState        gst then [Custom]             else [])
            ]
        emphasis = if not $ emphasisCheckedState gst
                   then NoEmphasis
                   else case emphasisValue gst of
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
            (if actionFilterCheckedState gst then [actionFilter] else []),
            (if complexityFilterCheckedState gst
             then [complexityFilter $ convertComplexityFilterValue $ complexityFilterValue gst]
             else [])
            ]
        rules = concat [
            (if reactionRuleCheckedState gst
             then [reactionRule $ convertReactionRuleValue $ reactionRuleValue gst]
             else []),
            (if trasherRuleCheckedState gst
             then [trasherRule $ convertTrasherRuleValue $ trasherRuleValue gst]
             else []),
            (if interactivityRuleCheckedState gst
             then [interactivityRule $ convertInteractivityRuleValue $ interactivityRuleValue gst]
             else []),
            (if costVarietyRuleCheckedState gst
             then [costVarietyRule]
             else [])
            ]
        colPlat = if colPlatAdditionCheckedState gst then RandomColPlat else NoColPlat
        shelters = if not $ sheltersAdditionCheckedState gst
                   then NoShelters
                   else case sheltersAdditionValue gst of
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
        convertInteractivityRuleValue v = v + 1

cardListCreate :: TreeView -> IO (ListStore CardListRow)
cardListCreate tv = do
    lst <- listStoreNew ([] :: [CardListRow])
    addColumn tv lst col1 "Name"
    addColumn tv lst col2 "Cost"
    addColumn tv lst col3 "Source"
    treeViewSetModel tv lst
    return lst
  where addColumn tv lst f name = do
            col <- treeViewColumnNew
            rend <- cellRendererTextNew
            treeViewColumnSetTitle col name
            treeViewColumnPackStart col rend True
            cellLayoutSetAttributes col rend lst (\row -> [ cellText := f row ])
            treeViewColumnSetExpand col True
            treeViewAppendColumn tv col

cardListAppend :: ListStore CardListRow -> Card -> IO ()
cardListAppend lst c = do
    listStoreAppend lst $ CardListRow {
        col1 = cardName c,
        col2 = displayAmount (cardCost c),
        col3 = show (cardSource c)
        }
    return ()

cardListRemove :: ListStore CardListRow -> Int -> IO ()
cardListRemove lst n = do
    listStoreRemove lst n
    return ()

{- Helper functions -}

-- | Gets all strings in the first column of a 'TreeModel'.
getAllCardNames :: ListStore CardListRow -> IO [String]
getAllCardNames lst = do
    size <- listStoreGetSize lst
    getAllValues size 0
  where getAllValues n i =
            if i >= n then return [] else do
                val <- listStoreGetValue lst i
                ((col1 val) :) `liftM` (getAllValues n (i+1))

-- | Gets a 'TreeModel' from a 'TreeView', throwing an error if no model exists.
getTreeModel :: TreeViewClass self => self -> IO TreeModel
getTreeModel tv = do
    tmm <- treeViewGetModel tv
    return $ maybe (error "No model defined for TreeView") id tmm

