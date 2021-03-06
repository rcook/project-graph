module ProjectGraph.Workday
    ( AbsentDays
    , addWorkdays
    , nearestWorkdayOnOrAfter
    , nextWorkday
    ) where

import           Data.Set (Set)
import qualified Data.Set as Set (member)
import           Data.Time (Day)
import           Data.Time.Calendar (addDays)
import           Data.Time.Calendar.WeekDate (toWeekDate)

type AbsentDays = Set Day

isWorkday :: AbsentDays -> Day -> Bool
isWorkday absentDays d
    | d `Set.member` absentDays = False
    | otherwise = let (_, _, n) = toWeekDate d in n < 6

nearestWorkdayOnOrAfter :: AbsentDays -> Day -> Day
nearestWorkdayOnOrAfter absentDays d
    | isWorkday absentDays d = d
    | otherwise = nextWorkday absentDays d

nextWorkday :: AbsentDays -> Day -> Day
nextWorkday absentDays d =
    let dayAfter = addDays 1 d
    in case isWorkday absentDays dayAfter of
        True -> dayAfter
        _ -> nextWorkday absentDays dayAfter

addWorkdays :: AbsentDays -> Int -> Day -> Day
addWorkdays absentDays n d
    | n < 0 = error "Must be positive"
    | n == 0 = d
    | otherwise = iterate (nextWorkday absentDays) d !! n
