{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}

module ProjectGraph.AppResources
    ( loadIcon
    , mainBuilder
    ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (hPut)
import           Data.FileEmbed (embedFile, embedStringFile, makeRelativeToProject)
import qualified Data.Text as Text (length)
import           GI.Gtk hiding (renderIcon)
import           Graphics.UI.Gtk
                    ( Pixbuf
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
    let xml = $(embedStringFile "ui/main.glade")
    builder <- builderNewFromString xml (fromIntegral $ Text.length xml)
    return builder
