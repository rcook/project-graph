{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module ProjectGraph.App (initApp) where

#if defined(darwin_HOST_OS)
import qualified Graphics.UI.Gtk as Gtk (initGUI, menuBarNew)
import qualified Graphics.UI.Gtk.OSX as OSX
                    ( applicationNew
                    , applicationReady
                    , applicationSetDockIconPixbuf
                    , applicationSetMenuBar
                    )
import           ProjectGraph.DataFiles (loadIcon)
#else
import           Control.Monad (void)
import qualified Graphics.UI.Gtk as Gtk (initGUI)
import qualified Graphics.UI.Gtk.Windows.Window as Window (windowSetDefaultIcon)
import           ProjectGraph.DataFiles (loadIcon)
import           System.Glib.Utils (setProgramName)
#endif

#if defined(darwin_HOST_OS)

initApp :: IO ()
initApp = do
    void Gtk.initGUI
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
    setProgramName "Project Graph"
    void Gtk.initGUI

#endif
