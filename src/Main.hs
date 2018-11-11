{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Error.Util (note)
import           Control.Monad (when)
import qualified Data.ByteString.Lazy as ByteString (writeFile)
import qualified Data.ByteString.Lazy.Char8 as Char8 (putStrLn)
import           Data.Char (toLower)
import           Data.Csv (ToField(..), ToRecord(..))
import qualified Data.Csv as Csv (encode, record)
import           Data.Foldable (foldlM, for_)
import           Data.Graph (edges)
import qualified Data.GraphViz as GraphViz (graphElemsToDot, printDotGraph, quickParams)
import           Data.List (foldl', intercalate)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (fromList, insert, lookup, mapWithKey, member)
import           Data.Maybe (catMaybes)
import qualified Data.Set as Set (fromList, union)
import qualified Data.Text.Lazy.IO as Text (writeFile)
import           Data.Time (Day)
import           Data.Yaml (decodeFileThrow)
import           Graphics.UI.Gtk (initGUI)
import           ProjectGraph.GUI.ProjectConfigChooser
                    ( ProjectConfig(ProjectConfig)
                    , chooseProjectConfig
                    )
import           ProjectGraph.GUI.ProjectWindow (display)
import           ProjectGraph.Options (Options(..), execWithOpts)
import           ProjectGraph.Planning (Plan(..), emptyPlan)
import           ProjectGraph.Schema
                    ( Availability(..)
                    , CommonAvailability(..)
                    , Group(..)
                    , Label(..)
                    , Person(..)
                    , PersonAvailability(..)
                    , Project
                    , Task(..)
                    )
import           ProjectGraph.TopSort (Dependency(..), graphAndOrder)
import           ProjectGraph.Workday
                    ( AbsentDays
                    , addWorkdays
                    , nearestWorkdayOnOrAfter
                    )
import           System.Exit (exitFailure)
import           System.FilePath (takeExtension)
import           System.IO (IOMode(..), hPutStrLn, withFile)
import           Text.Printf (printf)

type PeopleDays = Map Person Day

data OutputTarget =
    CSV FilePath
    | DOT FilePath
    | PlainText FilePath
    | GUI

outputTarget :: Maybe FilePath -> Maybe OutputTarget
outputTarget (Just path) =
    case map toLower (takeExtension path) of
        ".csv" -> Just $ CSV path
        ".dot" -> Just $ DOT path
        ".gv" -> Just $ DOT path
        ".txt" -> Just $ PlainText path
        _ -> Nothing
outputTarget Nothing = Just GUI

-- Plan

plan :: Project -> Plan
plan project =
    let plan = resolveTasks project
    in resolveRequires plan project

resolveTasks :: Project -> Plan
resolveTasks project = foldl' go emptyPlan project
    where go plan g =
            foldl'
                (\(Plan es tm ds) t ->
                    case label t of
                        Just l@(Label s) ->
                            case l `Map.member` tm of
                                True -> Plan (("Task label \"" ++ s ++ "\" is multiply defined") : es) tm ds
                                False -> Plan es (Map.insert l t tm) ds
                        _ -> Plan es tm ds)
                plan
                (tasks g)

resolveRequires :: Plan -> Project -> Plan
resolveRequires = foldl' go
    where go plan g =
            foldl'
                (\(Plan es tm ds) t ->
                    let (requiredTasks, ms) = foldl'
                                (\(ts, ms) l@(Label s) ->
                                    case Map.lookup l tm of
                                        Just t -> (t : ts, ms)
                                        _ -> (ts, (printf "Could not find task \"%s\"" s) : ms))
                                ([], [])
                                (requires t)
                        ds' = Dependency t requiredTasks : ds
                        es' = ms ++ es
                    in Plan es' tm ds')
                plan
                (tasks g)

type Schedule = [ScheduledTask]

data ScheduledTask = ScheduledTask
    { task :: Task
    , startDay :: Day
    , endDay :: Day
    }
    deriving Show

instance ToRecord ScheduledTask where
    toRecord (ScheduledTask t s e) =
        Csv.record
            [ toField $ tTitle t
            , toField $ effort t
            , toField $ unPerson <$> owner t
            , toField $ show s
            , toField $ show e
            ]
        where tTitle = title :: Task -> String

schedule :: Map Person AbsentDays -> PeopleDays -> [Task] -> Either String Schedule
schedule peopleMap peopleDays tasks = do
    (_, result) <- foldlM
                        (\(peopleDays', sts) t -> do
                            case (effort t, owner t) of
                                (Just eff, Just o) -> do
                                    let s = unPerson o
                                    startDay <- note ("Could not find person " ++ s) $ Map.lookup o peopleDays'
                                    absentDays <- note ("Could not find person " ++ s) $ Map.lookup o peopleMap
                                    let endDay = addWorkdays absentDays (eff - 1) startDay
                                        nextDay = addWorkdays absentDays 1 endDay
                                        peopleDays'' = Map.insert o nextDay peopleDays'
                                    return (peopleDays'', sts ++ [ScheduledTask t startDay endDay])
                                _ -> return (peopleDays', sts))
                        (peopleDays, [])
                        tasks
    return result

data Calendar = Calendar
    { peopleMap :: Map Person AbsentDays
    }
    deriving Show

resolvePersonAvailability :: AbsentDays -> PersonAvailability -> (Person, AbsentDays)
resolvePersonAvailability commonAbsentDays p = (person p, commonAbsentDays `Set.union` Set.fromList (paAbsentDays p))
    where paAbsentDays = absentDays :: PersonAvailability -> [Day]

resolveAvailability :: Availability -> Calendar
resolveAvailability (Availability c ps) =
    let commonAbsentDays = Set.fromList (caAbsentDays c)
    in Calendar (Map.fromList (map (resolvePersonAvailability commonAbsentDays) ps))
    where caAbsentDays = absentDays :: CommonAvailability -> [Day]

taskTitle :: Task -> String
taskTitle = title :: Task -> String

taskLabelCompact :: Task -> String
taskLabelCompact t =
    intercalate "\n" $ catMaybes
                        [ Just $ taskTitle t
                        , unPerson <$> owner t
                        , show <$> effort t
                        ]

main :: IO ()
main = execWithOpts $ \opts -> do
    initGUI
    case opts of
        Options (Just p) (Just a) (Just startDate) mbOutputPath -> mainWithOpts p a startDate mbOutputPath
        Options mbP mbA mbStartDate mbOutputPath ->
            chooseProjectConfig Nothing mbP mbA mbStartDate $ \(ProjectConfig p a startDate) ->
                mainWithOpts p a startDate mbOutputPath

mainWithOpts :: FilePath -> FilePath -> Day -> Maybe FilePath -> IO ()
mainWithOpts projectPath availabilityPath startDate mbOutputPath = do
    let target = case outputTarget mbOutputPath of
                    Just x -> x
                    _ -> error "Unsupported output format"

    availability <- decodeFileThrow availabilityPath

    let calendar = resolveAvailability availability

    project <- decodeFileThrow projectPath

    let peopleDays = Map.mapWithKey
                        (\_ absentDays -> nearestWorkdayOnOrAfter absentDays startDate)
                        (peopleMap calendar)
        Plan ms _ ds = plan project

    when (length ms > 0) $ do
        for_ ms putStrLn
        exitFailure

    let (g, indexed) = graphAndOrder ds
        result = case schedule (peopleMap calendar) peopleDays (map snd indexed) of
                Right x -> x
                Left s -> error s

    print result

    case target of
        CSV path -> ByteString.writeFile path $ Csv.encode result
        DOT path -> do
            let es = map (\(f, t) -> (f, t, "" :: String)) (edges g)
                dotGraph = GraphViz.graphElemsToDot
                    GraphViz.quickParams
                    (map (\(idx, task) -> (idx, taskLabelCompact task)) indexed)
                    es
            Text.writeFile path (GraphViz.printDotGraph dotGraph)
        PlainText path -> withFile path WriteMode
                    (\h -> for_ result $ \scheduledTask -> do
                                let t = task scheduledTask
                                    s = startDay scheduledTask
                                    e = endDay scheduledTask
                                hPutStrLn h $ printf "task: %s, effort: %s days, owner: %s, startDay: %s, endDay: %s"
                                                (taskTitle t)
                                                (show $ effort t)
                                                (show $ unPerson <$> owner t)
                                                (show s)
                                                (show e))
        GUI -> do
            Char8.putStrLn $ Csv.encode result
            let es = map (\(f, t) -> (f, t, "" :: String)) (edges g)
                dotGraph = GraphViz.graphElemsToDot
                    GraphViz.quickParams
                    (map (\(idx, task) -> (idx, taskLabelCompact task)) indexed)
                    es
            display dotGraph
