import Graphics.UI.Gtk

import Paths_dsgen
import Dsgen.Cards

main :: IO ()
main = do
    fn <- getDataFileName "res/conf/dominionCards.txt"
    cfr <- readCardFile fn
    either (\x -> print x) (\x -> print x) cfr
-- main = do
    -- initGUI
    -- builder <- builderNew
    -- gladeFilepath <- getDataFileName "res/gui/gui.glade"
    -- builderAddFromFile builder gladeFilepath
    -- window <- builderGetObject builder castToWindow "dsgenWindow"
    -- widgetShowAll window
    -- mainGUI

