module ProjectGraph.Options
    ( Options(..)
    , execWithOpts
    ) where

import           Data.Time (Day)
import           Options.Applicative
                    ( execParser
                    , info
                    , long
                    , maybeReader
                    , metavar
                    , option
                    , optional
                    , short
                    , strOption
                    )
import           ProjectGraph.DateUtil (parseDate)

data Options = Options
    { projectPath :: Maybe FilePath
    , availabilityPath :: Maybe FilePath
    , startDate :: Maybe Day
    , outputPath :: Maybe FilePath
    }

execWithOpts :: (Options -> IO ()) -> IO ()
execWithOpts f = execParser opts >>= f
    where
        parser = Options
            <$> (optional $ strOption (long "project" <> short 'p' <> metavar "PROJECTPATH"))
            <*> (optional $ strOption (long "availability" <> short 'a' <> metavar "AVAILABILITYPATH"))
            <*> (optional $ option (maybeReader parseDate) (long "start" <> short 's' <> metavar "STARTDATE"))
            <*> (optional $ strOption (long "output" <> short 'o' <> metavar "OUTPUTPATH"))
        opts = info parser mempty
