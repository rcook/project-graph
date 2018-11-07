module ProjectGraph.GUI.ProjectConfigChooser
    ( ProjectConfig(..)
    , chooseProjectConfig
    ) where

import           Control.Exception (bracket)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Data.Maybe (isJust)
import           Data.Time (Day)
import           Graphics.UI.Gtk
                    ( AttrOp(..)
                    , FileChooserAction(..)
                    , ResponseId(..)
                    , Window
                    , builderGetObject
                    , buttonPressEvent
                    , calendarDay
                    , calendarGetDate
                    , calendarMonth
                    , calendarYear
                    , castToButton
                    , castToCalendar
                    , castToDialog
                    , castToEntry
                    , dialogResponse
                    , dialogRun
                    , entryGetText
                    , entryText
                    , fileChooserAddFilter
                    , fileChooserDialogNew
                    , fileChooserGetFilename
                    , fileFilterAddPattern
                    , fileFilterNew
                    , fileFilterSetName
                    , on
                    , set
                    , widgetDestroy
                    , widgetSetSensitive
                    , windowTransientFor
                    )
import           ProjectGraph.AppResources (mainBuilder)
import           ProjectGraph.DateUtil (fromDate, toDate)
import           System.Glib.UTFString (glibToString, stringToGlib)

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

    dialog <- builderGetObject builder castToDialog "dlg"
    case mbWindow of
        Just window -> set dialog [ windowTransientFor := window ]
        _ -> return ()

    ctlProjectPath <- builderGetObject builder castToEntry "ctlProjectPath"
    case mbProjectPath of
        Just p -> set ctlProjectPath [ entryText := p]
        _ -> return ()

    ctlAvailabilityPath <- builderGetObject builder castToEntry "ctlAvailabilityPath"
    case mbAvailabilityPath of
        Just p -> set ctlAvailabilityPath [ entryText := p]
        _ -> return ()

    ctlStartDate <- builderGetObject builder castToCalendar "ctlStartDate"
    case mbStartDate of
        Just date -> do
            let (y, m, d) = fromDate date
            set ctlStartDate
                [ calendarYear := y
                , calendarMonth := m - 1
                , calendarDay := d
                ]
        _ -> return ()

    btnProjectPath <- builderGetObject builder castToButton "btnProjectPath"
    btnAvailabilityPath <- builderGetObject builder castToButton "btnAvailabilityPath"
    btnCancel <- builderGetObject builder castToButton "btnCancel"
    btnOK <- builderGetObject builder castToButton "btnOK"

    mbResult <- getProjectConfig ctlProjectPath ctlAvailabilityPath ctlStartDate
    widgetSetSensitive btnOK (isJust mbResult)

    on btnProjectPath buttonPressEvent $ lift $ do
        chooseOpenFile mbWindow "Select project file" $ \p ->
            set ctlProjectPath [ entryText := stringToGlib p ]
        mbResult <- getProjectConfig ctlProjectPath ctlAvailabilityPath ctlStartDate
        widgetSetSensitive btnOK (isJust mbResult)
        return True

    on btnAvailabilityPath buttonPressEvent $ lift $ do
        chooseOpenFile mbWindow "Select availability file" $ \p ->
            set ctlAvailabilityPath [ entryText := stringToGlib p ]
        mbResult <- getProjectConfig ctlProjectPath ctlAvailabilityPath ctlStartDate
        widgetSetSensitive btnOK (isJust mbResult)
        return True

    on btnCancel buttonPressEvent $ lift $ do
        dialogResponse dialog ResponseCancel
        return True

    on btnOK buttonPressEvent $ lift $ do
        dialogResponse dialog ResponseAccept
        return True

    responseId <- dialogRun dialog
    case responseId of
        ResponseAccept -> do
            Just result  <- getProjectConfig ctlProjectPath ctlAvailabilityPath ctlStartDate
            widgetDestroy dialog
            cb result
        _ -> widgetDestroy dialog

    where
        getProjectConfig ctlProjectPath ctlAvailabilityPath ctlStartDate = do
            projectPath <- glibToString <$> entryGetText ctlProjectPath
            availabilityPath <- glibToString <$> entryGetText ctlAvailabilityPath
            (y, m, d) <- calendarGetDate ctlStartDate
            let startDate = toDate y (m + 1) d
            if length projectPath > 0 && length availabilityPath > 0
                then return $ Just (ProjectConfig projectPath availabilityPath startDate)
                else return Nothing

chooseOpenFile ::
    Maybe Window
    -> String
    -> (FilePath -> IO ())
    -> IO ()
chooseOpenFile mbWindow title f =
    bracket
        (fileChooserDialogNew
            (Just title)
            mbWindow
            FileChooserActionOpen
                [ ("_Cancel", ResponseCancel)
                , ("_Open", ResponseAccept)
                ])
        widgetDestroy
        (\dialog -> do
            filter0 <- fileFilterNew
            fileFilterSetName filter0 "YAML files"
            fileFilterAddPattern filter0 "*.yaml"
            fileFilterAddPattern filter0 "*.yml"
            fileChooserAddFilter dialog filter0
            filter1 <- fileFilterNew
            fileFilterSetName filter1 "All files"
            fileFilterAddPattern filter1 "*"
            fileChooserAddFilter dialog filter1

            responseId <- dialogRun dialog
            case responseId of
                ResponseAccept -> do
                    mbFileName <- fileChooserGetFilename dialog
                    case mbFileName of
                        Just fileName -> f fileName
                        _ -> return ()
                _ -> return ()
        )
