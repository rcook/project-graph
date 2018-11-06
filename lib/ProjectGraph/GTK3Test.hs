module ProjectGraph.GTK3Test (display) where

import           Control.Monad.Trans.Class (lift)
import           Graphics.UI.Gtk
                    ( AttrOp(..)
                    , buttonLabel
                    , buttonNew
                    , buttonPressEvent
                    , containerAdd
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

    button <- buttonNew
    set button
        [ buttonLabel := "Close"
        ]

    on button buttonPressEvent $ lift $ do
        putStrLn "Quit app"
        mainQuit
        return True

    containerAdd window button

    windowSetDefaultSize window 512 512

    widgetShowAll window

    mainGUI
