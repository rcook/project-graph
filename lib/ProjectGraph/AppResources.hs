{-# LANGUAGE TemplateHaskell #-}

module ProjectGraph.AppResources
    ( loadIcon
    , mainBuilder
    ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (hPut)
import           Data.FileEmbed (embedFile, embedStringFile, makeRelativeToProject)
import           Graphics.UI.Gtk
                    ( Builder
                    , Pixbuf
                    , builderAddFromString
                    , builderNew
                    , pixbufNewFromFile
                    )
import           Language.Haskell.TH (Exp, Q)
import           System.IO (hClose)
import           System.IO.Temp (withSystemTempFile)

iconPath :: FilePath
iconPath = "ui/icon.png"

loadIcon :: Q Exp
loadIcon = [| renderIcon $(makeRelativeToProject iconPath >>= embedFile) |]

renderIcon :: ByteString -> IO Pixbuf
renderIcon bytes =
    withSystemTempFile "temp-icon" $ \path h -> do
        ByteString.hPut h bytes
        hClose h
        pixbufNewFromFile path

mainBuilder :: IO Builder
mainBuilder = do
    builder <- builderNew
    builderAddFromString builder ($(embedStringFile "ui/main.glade") :: String)
    return builder
