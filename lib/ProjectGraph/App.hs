{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module ProjectGraph.App (appTitle, initApp) where

#if defined(darwin_HOST_OS)
import           Foreign (castForeignPtr)
import           GI.GdkPixbuf
import           GI.Gtk
import qualified GI.Gtk as Gtk (init)
import qualified Graphics.UI.Gtk.OSX as OSX
                    ( applicationNew
                    , applicationReady
                    , applicationSetDockIconPixbuf
                    , applicationSetMenuBar
                    )
import qualified Graphics.UI.Gtk.Types as Gtk (MenuBar(..), Pixbuf(..))
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

coercePixbuf :: Pixbuf -> Gtk.Pixbuf
coercePixbuf (Pixbuf ptr) = (Gtk.Pixbuf . castForeignPtr . managedForeignPtr) ptr

coerceMenuBar :: MenuBar -> Gtk.MenuBar
coerceMenuBar (MenuBar ptr) = (Gtk.MenuBar . castForeignPtr . managedForeignPtr) ptr

initApp :: IO ()
initApp = do
    Gtk.init Nothing
    app <- OSX.applicationNew
    menuBar <- new MenuBar []
    OSX.applicationSetMenuBar app (coerceMenuBar menuBar)
    icon <- $loadIcon
    OSX.applicationSetDockIconPixbuf app (Just $ coercePixbuf icon)
    OSX.applicationReady app

#else

initApp :: IO ()
initApp = do
    icon <- $loadIcon
    windowSetDefaultIcon icon
    setProgramName appTitle
    void $ Gtk.init Nothing

#endif
