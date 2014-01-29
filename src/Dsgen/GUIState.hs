module Dsgen.GUIState (
    GUI(..),
    GUIState(..),
    readGUI,
    readGUIState
    ) where


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

    -- Card lists
    randomCardsTreeModel :: TreeModel,
    manualCardsTreeModel :: TreeModel,
    vetoedCardsTreeModel :: TreeModel
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
    }

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
    costVarietyRuleCB   <- liftIO $ builderGetObject b castToCheckButton "costVarietyRuleCheckButton"
    interactivityRuleCB <- liftIO $ builderGetObject b castToCheckButton "interactivityRuleCheckButton"
    reactionRuleCB      <- liftIO $ builderGetObject b castToCheckButton "reactionRuleCheckButton"
    reactionRuleCBX     <- liftIO $ builderGetObject b castToComboBox    "reactionRuleComboBox"
    trasherRuleCB       <- liftIO $ builderGetObject b castToCheckButton "trasherRuleCheckButton"
    trasherRuleCBX      <- liftIO $ builderGetObject b castToComboBox    "trasherRuleComboBox"

    -- Additions
    colPlatAdditionCB   <- liftIO $ builderGetObject b castToCheckButton "colPlatAdditionCheckButton"
    sheltersAdditionCB  <- liftIO $ builderGetObject b castToCheckButton "sheltersAdditionCheckButton"
    sheltersAdditionCBX <- liftIO $ builderGetObject b castToComboBox    "sheltersAdditionComboBox"

    -- Card lists
    randomCardsTreeView  <- liftIO $ builderGetObject b castToTreeView "randomCardsTreeView"
    randomCardsTreeModel <- getTreeModel randomCardsTreeView
    manualCardsTreeView  <- liftIO $ builderGetObject b castToTreeView "manualCardsTreeView"
    manualCardsTreeModel <- getTreeModel manualCardsTreeView
    vetoedCardsTreeView  <- liftIO $ builderGetObject b castToTreeView "vetoedCardsTreeView"
    vetoedCardsTreeModel <- getTreeModel vetoedCardsTreeView

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
        reactionRuleCheckButton      = reactionRuleCB,
        reactionRuleComboBox         = reactionRuleCBX,
        trasherRuleCheckButton       = trasherRuleCB,
        trasherRuleComboBox          = trasherRuleCBX,

        -- Additions
        colPlatAdditionCheckButton  = colPlatAdditionCB,
        sheltersAdditionCheckButton = sheltersAdditionCB,
        sheltersAdditionComboBox    = sheltersAdditionCB,

        -- Card lists
        randomCardsTreeModel = randomCardsTM,
        manualCardsTreeModel = manualCardsTM,
        vetoedCardsTreeModel = vetoedCardTM
        }

-- | Create a 'GUIState' object based on the contents of a 'GUI' object.
getGUIState :: GUI -> [Card] -> GUIState
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
    envoyCS         <- toggleButtonGetActive $ envoyCheckbutton         gui
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
    randomCardNames <- getAllCardNames $ randomCardsTreeModel gui
    let randomCL    = filter (\c -> elem c randomCardNames) cs
    manualCardNames <- getAllCardNames $ manualCardsTreeModel gui
    let manualCL    = filter (\c -> elem c manualCardNames) cs
    vetoedCardNames <- getAllCardNames $ vetoedCardsTreeModel gui
    let vetoedCL    = filter (\c -> elem c vetoedCardNames) cs

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
        randomCardsList = randomCardsCL,
        manualCardsList = manualCardsCL,
        vetoedCardsList = vetoedCardsCL
        }


{- Helper functions -}

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

