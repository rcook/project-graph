{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ProjectGraph.Schema
    ( Label(..)
    , Person(..)
    , Task(..)
    , TaskMap
    ) where

import           Data.Map.Strict (Map)
import qualified Data.Text as Text (unpack)
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
