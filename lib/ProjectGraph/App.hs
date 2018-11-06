{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module ProjectGraph.App (initApp) where

#if defined(darwin_HOST_OS)
import qualified Graphics.UI.Gtk as Gtk (menuBarNew)
import qualified Graphics.UI.Gtk.OSX as OSX
                    ( applicationNew
                    , applicationReady
                    , applicationSetDockIconPixbuf
                    , applicationSetMenuBar
                    )
#else
import qualified Graphics.UI.Gtk.Windows.Window as Window (windowSetDefaultIcon)
#endif
import           ProjectGraph.DataFiles (loadIcon)

#if defined(darwin_HOST_OS)

initApp :: IO ()
initApp = do
    app <- OSX.applicationNew
    menuBar <- Gtk.menuBarNew
    OSX.applicationSetMenuBar app menuBar
    icon <- $loadIcon
    OSX.applicationSetDockIconPixbuf app (Just icon)
    OSX.applicationReady app

#else

initApp :: IO ()
initApp = do
    icon <- $loadIcon
    Window.windowSetDefaultIcon (Just icon)

#endif
