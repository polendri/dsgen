import Control.Monad.Trans(liftIO)
import Graphics.UI.Gtk

import Paths_dsgen
import Dsgen.Cards

main :: IO ()
main = do
    -- fn <- getDataFileName "res/conf/dominionCards.txt"
    -- cfr <- readCardFile fn
    -- either (\x -> print x) (\x -> print x) cfr
    initGUI
    builder <- builderNew
    gladeFilepath <- getDataFileName "res/gui/gui.glade"
    builderAddFromFile builder gladeFilepath
    window <- builderGetObject builder castToWindow "dsgenWindow"

    -- Hook up signals
    on window deleteEvent (liftIO mainQuit >> return False)
    generateSetButton <- builderGetObject builder castToButton "generateSetButton"
    on generateSetButton buttonActivated generateSet
    outputTextView <- builderGetObject builder castToTextView "outputTextView"

    widgetShowAll window
    mainGUI

generateSet :: IO ()
generateSet = putStrLn "woo"

displayOutput :: IO ()
displayOutput = do
    buffer <- textBufferNew Nothing
    
