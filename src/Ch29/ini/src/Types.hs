module Types where

import           Data.Map                       ( Map )

newtype Header = Header String deriving (Eq, Ord, Show)

data Section = Section Header Assignments deriving (Eq, Show)

newtype Config = Config (Map Header Assignments) deriving (Eq, Show)

type Name = String
type Value = String
type Assignments = Map Name Value

type Extension = String
type Path = String

data File = File { path :: Path, filename :: FilePath , extension :: Extension } deriving (Show, Eq)

fullPath :: File -> FilePath
fullPath f = path f <> "/" <> filename f
