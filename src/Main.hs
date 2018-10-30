{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import           Control.Monad (when)
import           Data.Foldable (for_)
import           Data.List (foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (empty, fromList, insert, mapWithKey, member)
import           Data.Set (Set)
import qualified Data.Set as Set (fromList, union)
import           Data.String (IsString)
import           Data.Time (Day)
import           Data.Yaml (FromJSON, ToJSON, decodeFileThrow, encodeFile)
import           GHC.Generics (Generic)
import           Options.Applicative (ReadM, argument, execParser, info, maybeReader, metavar, str)
import           System.Exit (exitFailure, exitSuccess)
import           Text.Printf (printf)
import           Text.Read (readMaybe)

import ProjectGraph.DateUtil
import ProjectGraph.TopSort

type PeopleDays = Map Person Day

-- Command-line options

data Options = Options
    { projectPath :: FilePath
    , availabilityPath :: FilePath
    , startDate :: Day
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

type TaskMap = Map String Task

data Plan = Plan
    { messages :: [String]
    , taskMap :: TaskMap
    , deps :: [Dependency Task]
    }

empty :: Plan
empty = Plan [] Map.empty []

--

foo :: TaskMap -> Task -> Dependency Task
foo ts t = Dependency t (map (bar ts) (requires t))

bar :: TaskMap -> TaskLabel -> Task
bar ts (TaskLabel s) = unsafeLookUp s ts

resolveGroup :: Plan -> Group -> Plan
resolveGroup plan g =
    foldl'
        (\(Plan es ts' ds) t ->
            let ds' = foo ts' t : ds
            in case label t of
                Just (TaskLabel s) ->
                    case s `Map.member` ts' of
                        True -> Plan (("Task label \"" ++ s ++ "\" is multiply defined") : es) ts' ds'
                        False -> Plan es (Map.insert s t ts') ds'
                _ -> Plan es ts' ds')
        plan
        (tasks g)

plan :: Calendar -> Project -> Plan
plan c s = foldl' resolveGroup empty s

type Schedule = [ScheduledTask]

data ScheduledTask = ScheduledTask
    { task :: Task
    , startDay :: Day
    , endDay :: Day
    }
    deriving Show

schedule :: Map Person AbsentDays -> PeopleDays -> [Task] -> Schedule
schedule peopleMap peopleDays tasks =
    let (_, result) = foldl'
                        (\(peopleDays', sts) t ->
                            let o = owner t
                                eff = effort t
                                startDay = unsafeLookUp o peopleDays'
                                absentDays = unsafeLookUp o peopleMap
                                endDay = addWorkdays absentDays (eff - 1) startDay
                                nextDay = addWorkdays absentDays 1 endDay
                                peopleDays'' = Map.insert o nextDay peopleDays'
                            in (peopleDays'', sts ++ [ScheduledTask t startDay endDay])) -- ICK
                        (peopleDays, [])
                        tasks
    in result

main :: IO ()
main = execParser opts >>= runWithOpts
    where
        parser = Options
            <$> argument str (metavar "PROJECTPATH")
            <*> argument str (metavar "AVAILABILITYPATH")
            <*> argument (maybeReader parseDate) (metavar "STARTDATE")
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

runWithOpts :: Options -> IO ()
runWithOpts opts = do
    availability <- decodeFileThrow (availabilityPath opts)

    let calendar = resolveAvailability availability

    project <- decodeFileThrow (projectPath opts)

    let s = startDate opts
        peopleDays = Map.mapWithKey
                        (\_ absentDays -> nearestWorkdayOnOrAfter absentDays s)
                        (peopleMap calendar)
        Plan ms _ ds = plan calendar project

    when (length ms > 0) $ do
        for_ ms putStrLn
        exitFailure

    let is = leafIndices ds
        g = dependencyGraph is ds
        orderedTasks = map ((flip unsafeLookUp) (flipMap is)) (topSort g)

    let result = schedule (peopleMap calendar) peopleDays orderedTasks
    for_
        result $ \scheduledTask -> do
            let t = task scheduledTask
                s = startDay scheduledTask
                e = endDay scheduledTask
            putStrLn $ printf "task: %s, effort: %d days, owner: %s, startDay: %s, endDay: %s"
                            (tTitle t)
                            (effort t)
                            (show $ owner t)
                            (show s)
                            (show e)

    where tTitle = title :: Task -> String
