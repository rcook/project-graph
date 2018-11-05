{-# LANGUAGE TemplateHaskell #-}

module ProjectGraph.DataFiles (loadIcon) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (hPut)
import           Data.FileEmbed (embedFile, makeRelativeToProject)
import           Graphics.UI.Gtk (Pixbuf, pixbufNewFromFile)
import           Language.Haskell.TH (Exp, Q)
import           System.IO (hClose)
import           System.IO.Temp (withSystemTempFile)

iconPath :: FilePath
iconPath = "icon.png"

loadIcon :: Q Exp
loadIcon = [| renderIcon $(makeRelativeToProject iconPath >>= embedFile) |]

renderIcon :: ByteString -> IO Pixbuf
renderIcon bytes =
    withSystemTempFile iconPath $ \path h -> do
        ByteString.hPut h bytes
        hClose h
        pixbufNewFromFile path
