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
    gui <- readGUI builder

    -- Load cards
    cardFiles <- cardFileNames
    cardse <- runErrorT $ readCardFiles cardFiles
    case cardse of
        Left s -> startupErrorQuit $ show s
        Right cs -> do
            -- Hook up signals
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

