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
import           ProjectGraph.DataFiles (loadIcon)
#endif

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
initApp = return ()

#endif
