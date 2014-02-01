import Control.Monad.Error
import Control.Monad.Trans(liftIO)
import Data.IORef(IORef, newIORef, readIORef, writeIORef)
import Data.List(sort)
import Data.Maybe(fromJust)
import Graphics.UI.Gtk
import System.IO.Unsafe(unsafePerformIO)
import System.Random

import Paths_dsgen
import Dsgen.Cards
import Dsgen.GUIState
import Dsgen.SetSelect

-- Global variable for holding row indices which should be deleted
rowsToDelete :: IORef [Int]
{-# NOINLINE rowsToDelete #-}
rowsToDelete = unsafePerformIO (newIORef [])

main :: IO ()
main = do
    -- GUI initializations
    initGUI
    builder <- builderNew
    gladeFilepath <- getDataFileName "res/gui/gui.glade"
    builderAddFromFile builder gladeFilepath
    window <- builderGetObject builder castToWindow "mainWindow"
    on window deleteEvent (liftIO mainQuit >> return False)

    -- Get widgets
    gui <- readGUI builder

    -- Load cards
    cardFiles <- cardFileNames
    cardse <- runErrorT $ readCardFiles cardFiles
    case cardse of
        Left s -> startupErrorQuit $ show s
        Right cs -> do
            -- Hook up signals
            hookSignals gui cs

            -- Start the GUI loop
            widgetShowAll window
            mainGUI

hookSignals :: GUI -> [Card] -> IO ()
hookSignals gui cs = do
    on (selectSetButton gui) buttonActivated $ fillSelection gui cs
    on (rmvManualCardsButton gui) buttonActivated $ removeCards (manualCardsTreeView gui)
                                                                (manualCardsListStore gui)
    on (rmvVetoedCardsButton gui) buttonActivated $ removeCards (vetoedCardsTreeView gui)
                                                                (vetoedCardsListStore gui)
    return ()
    hookCardListDrag (randomCardsTreeView gui) (randomCardsListStore gui)
    hookCardListDrag (manualCardsTreeView gui) (manualCardsListStore gui)
    hookCardListDrag (vetoedCardsTreeView gui) (vetoedCardsListStore gui)
    return ()
  where hookCardListDrag tv ls = do
            tm <- fromJust `liftM` treeViewGetModel tv
            ts <- treeViewGetSelection tv
            -- treeSelectionSetMode ts SelectionMultiple
            textAtom <- atomNew "text/plain"
            tl <- targetListNew
            targetListAdd tl textAtom [TargetSameApp] 0
            treeViewEnableModelDragDest tv tl [ActionCopy, ActionMove]
            treeViewEnableModelDragSource tv [Button1] tl [ActionMove]
            on tv dragDataGet $ \_ _ _ -> do
                selectionList <- liftIO $ treeSelectionGetSelectedRows ts
                let selectedRows = sort $ map head selectionList
                selectedCards <- liftIO $ mapM (listStoreGetValue ls) selectedRows
                selectionDataSetText $ show selectedCards
                liftIO $ writeIORef rowsToDelete selectedRows
                return ()
            on tv dragDataReceived $ \ctx _ _ time -> do
                text <- fromJust `liftM` selectionDataGetText
                let rows = read text :: [CardListRow]
                liftIO $ sequence_ $ map (listStoreAppend ls) rows
                liftIO $ dragFinish ctx True True time
                return ()
            on tv dragDataDelete $ \_ -> do
                rows <- readIORef rowsToDelete
                let sortedRows = reverse $ sort rows
                sequence_ $ map (listStoreRemove ls) sortedRows

-- | Generate a random selection of cards and display it
fillSelection :: GUI -> [Card] -> IO ()
fillSelection gui cs = do
    notebookSetCurrentPage (mainNotebook gui) 1
    gst <- getGUIState gui cs
    let ssos = mkSetSelectOptions gst cs
    sgre <- runErrorT $ selectSet ssos
    case sgre of
        Left e -> putStrLn e -- TODO: use status bar!
        Right sgr -> do
            let lst = randomCardsListStore gui
            listStoreClear lst
            mapM_ (cardListAppend lst) (ssrKingdomCards sgr)
            labelSetText (colPlatLabel gui)  (if ssrUsesColPlat sgr then "Yes" else "No")
            labelSetText (sheltersLabel gui) (if ssrUsesShelters sgr then "Yes" else "No")

-- | Remove selected cards from a 'TreeView' to another 'ListStore'
removeCards :: TreeViewClass self => self -> ListStore CardListRow -> IO ()
removeCards tv ls = do
    ts <- treeViewGetSelection tv
    selectionList <- treeSelectionGetSelectedRows ts
    let selectedRows = sort $ map head selectionList
    sequence_ $ map (listStoreRemove ls) $ reverse $ selectedRows
    treeSelectionUnselectAll ts

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

