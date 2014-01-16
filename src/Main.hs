import Control.Monad.Trans(liftIO)
import Graphics.UI.Gtk

import Paths_dsgen
import Dsgen.Cards

main :: IO ()
main = do
    initGUI
    builder <- builderNew
    gladeFilepath <- getDataFileName "res/gui/gui.glade"
    builderAddFromFile builder gladeFilepath
    window <- builderGetObject builder castToWindow "dsgenWindow"

    -- Hook up signals
    on window deleteEvent (liftIO mainQuit >> return False)
    generateSetButton <- builderGetObject builder castToButton "generateSetButton"
    on generateSetButton buttonActivated $ generateSet builder
    outputTextView <- builderGetObject builder castToTextView "outputTextView"

    widgetShowAll window
    mainGUI

generateSet :: Builder -> IO ()
generateSet builder = displayOutput builder "woo"

-- displayCardOutput :: Builder -> IO ()
-- display

displayOutput :: Builder -> String -> IO ()
displayOutput builder s = do
    otv <- builderGetObject builder castToTextView "outputTextView"
    buf <- textViewGetBuffer otv
    textBufferSetText buf s
    
