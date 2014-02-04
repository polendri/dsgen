module Dsgen.ChecklistDialog (
    getListSelection
    ) where

import Graphics.UI.Gtk

{- | Given a list of items, 'getListSelection' opens a dialog which prompts
the user to select which items they would like to select, and returns the
resulting sublist of selected items. -}
getListSelection :: WindowClass w =>
    w                -- ^ The transient window for the dialog.
    -> (a -> String) -- ^ A function to convert items to displayable strings.
    -> Bool          -- ^ If true, checkboxes should be pre-toggled.
    -> [a]           -- ^ The list of items.
    -> IO [a]        -- ^ The list of items selected by the user.
getListSelection w disp toggled xs = do
    d <- dialogNew
    windowSetTransientFor d w
    dialogAddButton d "Ok" ResponseOk
    dVBox <- dialogGetUpper d
    scroller <- scrolledWindowNew Nothing Nothing
    boxPackStart dVBox scroller PackNatural 0
    vBox <- vBoxNew True 0
    containerAdd scroller vBox
    sequence_ $ for xs $ (addToggle toggled vBox) . disp
  where addToggle toggled box x = do
            t <- toggleButtonNewWithLabel x
            boxPackStart box t PackNatural 0

