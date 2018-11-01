{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main (main) where

import           Control.Error.Util (note)
import           Control.Monad (when)
import qualified Data.ByteString.Lazy as ByteString (writeFile)
import           Data.Char (toLower)
import           Data.Csv (ToField(..), ToRecord(..))
import qualified Data.Csv as Csv (encode, record)
import           Data.Foldable (foldlM, for_)
import           Data.Graph (edges, vertices)
import qualified Data.GraphViz as GraphViz (graphElemsToDot, printDotGraph, quickParams)
import           Data.List (foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (empty, fromList, insert, lookup, mapWithKey, member)
import           Data.Set (Set)
import qualified Data.Set as Set (fromList, union)
import           Data.String (IsString)
import qualified Data.Text.Lazy.IO as Text (writeFile)
import           Data.Time (Day)
import           Data.Yaml (FromJSON, ToJSON, decodeFileThrow, encodeFile)
import           GHC.Generics (Generic)
import           Options.Applicative (ReadM, argument, execParser, info, maybeReader, metavar, str)
import           System.Exit (exitFailure, exitSuccess)
import           System.FilePath (takeExtension)
import           System.IO (IOMode(..), hPutStrLn, withFile)
import           Text.Printf (printf)
import           Text.Read (readMaybe)

import ProjectGraph.DateUtil
import ProjectGraph.TopSort

type PeopleDays = Map Person Day

data OutputFormat = CSV | DOT | Text

outputFormat :: FilePath -> Maybe OutputFormat
outputFormat p =
    case map toLower (takeExtension p) of
        ".csv" -> Just CSV
        ".dot" -> Just DOT
        ".txt" -> Just Text
        _ -> Nothing

-- Command-line options

data Options = Options
    { projectPath :: FilePath
    , availabilityPath :: FilePath
    , startDate :: Day
    , outputPath :: FilePath
    }

-- YAML serialization

type Project = [Group]

type Days = Int

newtype TaskLabel = TaskLabel String deriving (Eq, FromJSON, Generic, Ord, Show, ToJSON)

newtype Person = Person String deriving (Eq, FromJSON, Generic, Ord, Show, ToJSON)

data Group = Group
    { title :: String
    , description :: Maybe String
    , tasks :: [Task]
    }
    deriving (Eq, FromJSON, Generic, Show, ToJSON)

data Task = Task
    { title :: String
    , description :: Maybe String
    , label :: Maybe TaskLabel
    , effort :: Days
    , owner :: Person
    , requires :: [TaskLabel]
    }
    deriving (Eq, FromJSON, Generic, Ord, Show, ToJSON)

-- Plan

type TaskMap = Map TaskLabel Task

data Plan = Plan
    { messages :: [String]
    , taskMap :: TaskMap
    , deps :: [Dependency Task]
    }

empty :: Plan
empty = Plan [] Map.empty []

--

plan :: Project -> Plan
plan project =
    let plan = resolveTasks project
    in resolveRequires plan project

resolveTasks :: Project -> Plan
resolveTasks project = foldl' go empty project
    where go plan g =
            foldl'
                (\(Plan es tm ds) t ->
                    case label t of
                        Just l@(TaskLabel s) ->
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
                                (\(ts, ms) l@(TaskLabel s) ->
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
            [ toField (tTitle t)
            , toField (effort t)
            , let Person s = owner t in toField s
            , toField (show s)
            , toField (show e)
            ]
        where tTitle = title :: Task -> String

schedule :: Map Person AbsentDays -> PeopleDays -> [Task] -> Either String Schedule
schedule peopleMap peopleDays tasks = do
    (_, result) <- foldlM
                        (\(peopleDays', sts) t -> do
                            let o@(Person s) = owner t
                                eff = effort t
                            startDay <- note ("Could not find person " ++ s) $ Map.lookup o peopleDays'
                            absentDays <- note ("Could not find person " ++ s) $ Map.lookup o peopleMap
                            let endDay = addWorkdays absentDays (eff - 1) startDay
                                nextDay = addWorkdays absentDays 1 endDay
                                peopleDays'' = Map.insert o nextDay peopleDays'
                            return (peopleDays'', sts ++ [ScheduledTask t startDay endDay])) -- ICK
                        (peopleDays, [])
                        tasks
    return result

main :: IO ()
main = execParser opts >>= runWithOpts
    where
        parser = Options
            <$> argument str (metavar "PROJECTPATH")
            <*> argument str (metavar "AVAILABILITYPATH")
            <*> argument (maybeReader parseDate) (metavar "STARTDATE")
            <*> argument str (metavar "OUTPUTPATH")
        opts = info parser mempty

data CommonAvailability = CommonAvailability
    { absentDays :: [Day]
    }
    deriving (FromJSON, Generic, Show, ToJSON)

data PersonAvailability = PersonAvailability
    { person :: Person
    , absentDays :: [Day]
    }
    deriving (FromJSON, Generic, Show, ToJSON)

data Availability = Availability
    { common :: CommonAvailability
    , people :: [PersonAvailability]
    }
    deriving (FromJSON, Generic, Show, ToJSON)

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
taskLabelCompact task =
    printf
        "%s\n%s\neffort: %d"
        (taskTitle task)
        (let Person s = owner task in s)
        (effort task)

runWithOpts :: Options -> IO ()
runWithOpts opts = do
    let p = outputPath opts
        fmt = case outputFormat p of
                Just x -> x
                _ -> error "Unsupported output format"

    availability <- decodeFileThrow (availabilityPath opts)

    let calendar = resolveAvailability availability

    project <- decodeFileThrow (projectPath opts)

    let s = startDate opts
        peopleDays = Map.mapWithKey
                        (\_ absentDays -> nearestWorkdayOnOrAfter absentDays s)
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

    case fmt of
        CSV -> ByteString.writeFile p (Csv.encode result)
        DOT -> do
            let es = map (\(f, t) -> (f, t, "")) (edges g)
                dotGraph = GraphViz.graphElemsToDot
                    GraphViz.quickParams
                    (map (\(idx, task) -> (idx, taskLabelCompact task)) indexed)
                    es
            Text.writeFile p (GraphViz.printDotGraph dotGraph)
        Text -> withFile p WriteMode
                    (\h -> for_ result $ \scheduledTask -> do
                                let t = task scheduledTask
                                    s = startDay scheduledTask
                                    e = endDay scheduledTask
                                hPutStrLn h $ printf "task: %s, effort: %d days, owner: %s, startDay: %s, endDay: %s"
                                                (taskTitle t)
                                                (effort t)
                                                (let Person s = owner t in s)
                                                (show s)
                                                (show e))