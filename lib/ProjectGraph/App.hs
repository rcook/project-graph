{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module ProjectGraph.App (appTitle, initApp) where

#if defined(darwin_HOST_OS)
import           Control.Monad (void)
import           GI.Gtk
import qualified Graphics.UI.Gtk as Gtk (initGUI, menuBarNew)
import qualified Graphics.UI.Gtk.OSX as OSX
                    ( applicationNew
                    , applicationReady
                    , applicationSetDockIconPixbuf
                    , applicationSetMenuBar
                    )
import           ProjectGraph.AppResources (loadIcon)
#else
import           Control.Monad (void)
import           GI.Gtk
import qualified GI.Gtk as Gtk (init)
import           ProjectGraph.AppResources (loadIcon)
import           System.Glib.Utils (setProgramName)
#endif

appTitle :: String
appTitle = "Project Graph"

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
    windowSetDefaultIcon icon
    setProgramName appTitle
    void $ Gtk.init Nothing

#endif
