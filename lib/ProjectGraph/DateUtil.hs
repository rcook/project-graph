module ProjectGraph.DateUtil
    ( fromDate
    , parseDate
    , toDate
    ) where

import           Data.List.Split (splitOn)
import           Data.Time (Day, fromGregorian, toGregorian)
import           Text.Read (readMaybe)

parseDate :: String -> Maybe Day
parseDate s =
    case splitOn "-" s of
        sy : sm : sd : [] -> do
            y <- readMaybe sy
            m <- readMaybe sm
            d <- readMaybe sd
            return $ fromGregorian y m d
        _ -> Nothing

toDate :: Int -> Int -> Int -> Day
toDate y = fromGregorian (fromIntegral y)

fromDate :: Day -> (Int, Int, Int)
fromDate date = let (y, m, d) = toGregorian date in (fromIntegral y, m, d)
