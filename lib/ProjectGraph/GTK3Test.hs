module ProjectGraph.GTK3Test (display) where

import           Graphics.UI.Gtk
                    ( AttrOp(..)
                    , initGUI
                    , mainGUI
                    , mainQuit
                    , objectDestroy
                    , on
                    , set
                    , widgetShowAll
                    , windowNew
                    , windowSetDefaultSize
                    , windowTitle
                    )
import           ProjectGraph.App (initApp)

display :: IO ()
display = do
    initGUI

    initApp

    window <- windowNew

    set window
        [ windowTitle := "Project Graph"
        ]

    on window objectDestroy $ do
        mainQuit
        return ()

    windowSetDefaultSize window 512 512

    widgetShowAll window

    mainGUI
