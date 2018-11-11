{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module ProjectGraph.Schema
    ( Availability(..)
    , CommonAvailability(..)
    , Group(..)
    , Label(..)
    , Person(..)
    , PersonAvailability(..)
    , Project
    , Task(..)
    , TaskMap
    ) where

import           Data.Map.Strict (Map)
import qualified Data.Text as Text (unpack)
import           Data.Time (Day)
import           Data.Yaml
                    ( FromJSON(..)
                    , ToJSON(..)
                    , (.:)
                    , (.:?)
                    , (.!=)
                    , withObject
                    , withText
                    )
import           GHC.Generics (Generic)

type Days = Int

newtype Person = Person
    { unPerson :: String
    }
    deriving (Eq, Generic, Ord, Show)

instance FromJSON Person where
    parseJSON = withText "person" $ \s -> return $ Person (Text.unpack s)

instance ToJSON Person where
    toJSON (Person s) = toJSON s

newtype Label = Label
    { unLabel :: String
    }
    deriving (Eq, Generic, Ord, Show)

instance FromJSON Label where
    parseJSON = withText "label" $ \s -> return $ Label (Text.unpack s)

instance ToJSON Label where
    toJSON (Label s) = toJSON s

data Task = Task
    { title :: String
    , description :: Maybe String
    , label :: Maybe Label
    , effort :: Maybe Days
    , owner :: Maybe Person
    , requires :: [Label]
    }
    deriving (Eq, Generic, Ord, Show, ToJSON)

instance FromJSON Task where
    parseJSON = withObject "task" $ \o -> do
        title <- o .: "title"
        description <- o .:? "description"
        label <- o .:? "label"
        effort <- o .:? "effort"
        owner <- o .:? "owner"
        requires <- o .:? "requires" .!= []
        return $ Task title description label effort owner requires

type TaskMap = Map Label Task

type Project = [Group]

data Group = Group
    { title :: String
    , description :: Maybe String
    , tasks :: [Task]
    }
    deriving (Eq, Generic, Show, ToJSON)

instance FromJSON Group where
    parseJSON = withObject "group" $ \o -> do
        title <- o .: "title"
        description <- o .:? "description"
        tasks <- o .:? "tasks" .!= []
        return $ Group title description tasks

data CommonAvailability = CommonAvailability
    { absentDays :: [Day]
    }
    deriving (Generic, Show, ToJSON)

instance FromJSON CommonAvailability where
    parseJSON = withObject "commonAvailability" $ \o -> do
        absentDays <- o .:? "absentDays" .!= []
        return $ CommonAvailability absentDays

data PersonAvailability = PersonAvailability
    { person :: Person
    , absentDays :: [Day]
    }
    deriving (Generic, Show, ToJSON)

instance FromJSON PersonAvailability where
    parseJSON = withObject "personAvailability" $ \o -> do
        person <- o .: "person"
        absentDays <- o .:? "absentDays" .!= []
        return $ PersonAvailability person absentDays

data Availability = Availability
    { common :: CommonAvailability
    , people :: [PersonAvailability]
    }
    deriving (Generic, Show, ToJSON)

instance FromJSON Availability where
    parseJSON = withObject "availability" $ \o -> do
        common <- o .:? "common" .!= CommonAvailability []
        people <- o .:? "people" .!= []
        return $ Availability common people
