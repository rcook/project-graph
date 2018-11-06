module ProjectGraph.GTK3Test (display) where

import           Control.Monad.Trans.Class (lift)
import           Graphics.UI.Gtk
                    ( AttrOp(..)
                    , buttonLabel
                    , buttonNew
                    , buttonPressEvent
                    , containerAdd
                    , mainGUI
                    , mainQuit
                    , objectDestroy
                    , on
                    , set
                    , widgetShowAll
                    , windowDefaultHeight
                    , windowDefaultWidth
                    , windowNew
                    , windowTitle
                    )
import           ProjectGraph.App (initApp)

display :: IO ()
display = do
    initApp

    window <- windowNew

    set window
        [ windowDefaultHeight := 512
        , windowDefaultWidth := 512
        , windowTitle := "Project Graph"
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

    widgetShowAll window

    mainGUI
