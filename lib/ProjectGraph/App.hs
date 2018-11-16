{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module ProjectGraph.App
    ( appTitle
    , gtkMain
    , initApp
    ) where

#if defined(darwin_HOST_OS)
import           Control.Monad (void)
import           Foreign (castForeignPtr)
import           GI.GdkPixbuf
import           GI.Gtk
import qualified GI.Gtk as Gtk (init, main)
import qualified Graphics.UI.Gtk.OSX as OSX
                    ( applicationNew
                    , applicationReady
                    , applicationSetDockIconPixbuf
                    , applicationSetMenuBar
                    )
import qualified Graphics.UI.Gtk.Types as Gtk (MenuBar(..), Pixbuf(..))
import           ProjectGraph.AppResources (loadIcon)
import           System.Posix.Escape (escape)
import           System.Posix.Process (getProcessID)
import           System.Process (system)
import           Text.Printf (printf)
#else
import           Control.Monad (void)
import           GI.Gtk
import qualified GI.Gtk as Gtk (init, main)
import           ProjectGraph.AppResources (loadIcon)
import           System.Glib.Utils (setProgramName)
#endif

appTitle :: String
appTitle = "Project Graph"

#if defined(darwin_HOST_OS)

coercePixbuf :: Pixbuf -> Gtk.Pixbuf
coercePixbuf (Pixbuf ptr) = (Gtk.Pixbuf . castForeignPtr . managedForeignPtr) ptr

coerceMenuBar :: MenuBar -> Gtk.MenuBar
coerceMenuBar (MenuBar ptr) = (Gtk.MenuBar . castForeignPtr . managedForeignPtr) ptr

setAppTitle :: String -> IO ()
setAppTitle title = do
    pid <- getProcessID
    void $ system $ printf
                    "/usr/bin/lsappinfo setinfo %s --name %s"
                    (show pid)
                    (escape title)

initApp :: IO ()
initApp = do
    Gtk.init Nothing

    app <- OSX.applicationNew

    menuBar <- new MenuBar []
    OSX.applicationSetMenuBar app (coerceMenuBar menuBar)

    icon <- $loadIcon
    OSX.applicationSetDockIconPixbuf app (Just $ coercePixbuf icon)
    OSX.applicationReady app

gtkMain :: IO ()
gtkMain = setAppTitle appTitle >> Gtk.main

#else

initApp :: IO ()
initApp = do
    icon <- $loadIcon
    windowSetDefaultIcon icon
    setProgramName appTitle
    void $ Gtk.init Nothing

gtkMain :: IO ()
gtkMain = Gtk.main

#endif
