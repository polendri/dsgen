import Control.Monad.Error
import Control.Monad.Trans(liftIO)
import Data.List
import Graphics.UI.Gtk

import Paths_dsgen
import Dsgen.Cards
import Dsgen.SetGen

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
    cardse <- runErrorT readCardFiles
    case cardse of
        Left s -> errorQuit window $ show s
        Right cs -> putStrLn "woo"

    -- Hook up signals
    generateSetButton <- builderGetObject builder castToButton "generateSetButton"
    on generateSetButton buttonActivated $ generateSet builder
    outputTextView <- builderGetObject builder castToTextView "outputTextView"

    widgetShowAll window
    mainGUI

generateSet :: Builder -> IO ()
generateSet builder = do
    -- sgre <- generateSet
    displayOutput builder "woo"

-- displayCardOutput :: Builder -> [Card] -> IO ()
-- displayCardOutput b cs = displayOutput b $ intercalate ", " $ map showCardName cs

displayOutput :: Builder -> String -> IO ()
displayOutput builder s = do
    otv <- builderGetObject builder castToTextView "outputTextView"
    buf <- textViewGetBuffer otv
    textBufferSetText buf s

errorQuit :: Window -> String -> IO ()
errorQuit w s = do
    dialog <- messageDialogNew (Just w)
                               [DialogModal, DialogDestroyWithParent]
                               MessageError
                               ButtonsOk
                               s
    dialogRun dialog
    mainQuit
    
