{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module ProjectGraph.GUI.ProjectConfigChooser
    ( ProjectConfig(..)
    , chooseProjectConfig
    ) where

import           Data.Maybe (fromJust, isJust)
import qualified Data.Text as Text (pack, unpack)
import           Data.Time (Day)
import           GHC.Int (Int32)
import           GI.Gtk
import           ProjectGraph.AppResources (mainBuilder)
import           ProjectGraph.DateUtil (fromDate, toDate)

responseAccept :: Int32
responseAccept = -3
responseCancel = -6

data ProjectConfig = ProjectConfig
    { projectPath :: FilePath
    , availabilityPath :: FilePath
    , startDate :: Day
    }

chooseProjectConfig ::
    Maybe Window
    -> Maybe FilePath
    -> Maybe FilePath
    -> Maybe Day
    -> (ProjectConfig -> IO ())
    -> IO ()
chooseProjectConfig mbWindow mbProjectPath mbAvailabilityPath mbStartDate cb = do
    builder <- mainBuilder
    dialog <- #getObject builder "dlg" >>= unsafeCastTo Dialog . fromJust
    case mbWindow of
        Just window -> set dialog [ #transientFor := window ]
        _ -> return ()

    ctlProjectPath <- #getObject builder "ctlProjectPath" >>= unsafeCastTo Entry . fromJust
    case mbProjectPath of
        Just p -> set ctlProjectPath [ #text := Text.pack p]
        _ -> return ()

    ctlAvailabilityPath <- #getObject builder "ctlAvailabilityPath" >>= unsafeCastTo Entry . fromJust
    case mbAvailabilityPath of
        Just p -> set ctlAvailabilityPath [ #text := Text.pack p]
        _ -> return ()

    ctlStartDate <- #getObject builder "ctlStartDate" >>= unsafeCastTo Calendar . fromJust
    case mbStartDate of
        Just date -> do
            let (y, m, d) = fromDate date
            set ctlStartDate
                [ #year := fromIntegral y
                , #month := fromIntegral $ m - 1
                , #day := fromIntegral d
                ]
        _ -> return ()

    btnProjectPath <- #getObject builder "btnProjectPath" >>= unsafeCastTo Button . fromJust
    btnAvailabilityPath <- #getObject builder "btnAvailabilityPath" >>= unsafeCastTo Button . fromJust
    btnCancel <- #getObject builder "btnCancel" >>= unsafeCastTo Button . fromJust
    btnOK <- #getObject builder "btnOK" >>= unsafeCastTo Button . fromJust

    mbResult <- getProjectConfig ctlProjectPath ctlAvailabilityPath ctlStartDate
    #setSensitive btnOK (isJust mbResult)

    on btnProjectPath #clicked $ do
        chooseOpenFile mbWindow "Select project file" $ \p ->
            set ctlProjectPath [ #text := Text.pack p ]
        mbResult <- getProjectConfig ctlProjectPath ctlAvailabilityPath ctlStartDate
        #setSensitive btnOK (isJust mbResult)

    on btnAvailabilityPath #clicked $ do
        chooseOpenFile mbWindow "Select availability file" $ \p ->
            set ctlAvailabilityPath [ #text := Text.pack p ]
        mbResult <- getProjectConfig ctlProjectPath ctlAvailabilityPath ctlStartDate
        #setSensitive btnOK (isJust mbResult)

    on btnCancel #clicked $ #response dialog responseCancel

    on btnOK #clicked $ #response dialog responseAccept

    responseId <- #run dialog
    if responseId == responseAccept
        then do
            Just result  <- getProjectConfig ctlProjectPath ctlAvailabilityPath ctlStartDate
            #destroy dialog
            cb result
        else #destroy dialog

    where
        getProjectConfig :: Entry -> Entry -> Calendar -> IO (Maybe ProjectConfig)
        getProjectConfig ctlProjectPath ctlAvailabilityPath ctlStartDate = do
            projectPath <- Text.unpack <$> #getText ctlProjectPath
            availabilityPath <- Text.unpack <$> #getText ctlAvailabilityPath
            (y, m, d) <- #getDate ctlStartDate
            let startDate = toDate (fromIntegral y) (fromIntegral m + 1) (fromIntegral d)
            if length projectPath > 0 && length availabilityPath > 0
                then return $ Just (ProjectConfig projectPath availabilityPath startDate)
                else return Nothing

chooseOpenFile ::
    Maybe Window
    -> String
    -> (FilePath -> IO ())
    -> IO ()
chooseOpenFile mbWindow title cb = do
    dialog <- new FileChooserNative [ #action := FileChooserActionOpen ]

    fileChooser <- toFileChooser dialog

    filter0 <- new FileFilter []
    #setName filter0 $ Just "YAML files"
    #addPattern filter0 "*.yaml"
    #addPattern filter0 "*.yml"
    #addFilter fileChooser filter0
    filter1 <- new FileFilter []
    #setName filter1 $ Just "All files"
    #addPattern filter1 "*"
    #addFilter fileChooser filter1

    responseId <- #run dialog
    if responseId == responseAccept
        then do
            c <- toFileChooser dialog
            mbPath <- #getFilename c
            case mbPath of
                Just p -> cb p
                _ -> return ()
        else return ()
