{-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE OverloadedStrings #-} -- would get rid of T.pack
module Cellout.Types
    ( Notebook(..)
    , Cell(..)
    , CommonCellContent(..)
    , Output(..)
    , ExecutionCount(..)
    , MimeBundle
    , StreamName(..)
    , Metadata
    ) where

import Control.Arrow -- for >>>
import Data.Aeson
import Data.Set (Set, empty)
import Data.Text.Encoding
import GHC.Generics
import Text.ParserCombinators.ReadP
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Lazy as HML
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import System.Environment (getArgs)


{- |
   In-memory representation of a Jupyter notebook.
 -}
data Notebook =
    Notebook
    { cells :: [ Cell ]
    , nbmetadata :: Metadata -- TODO: may want to be more specific about typical keys in the notebook metadata (kernelspec, language_info)
    , nbformat :: Int
    , nbformat_minor :: Int
    } deriving (Show, Generic, Eq)

-- | convenience function for creating a Notebook with nbformat 4.2
notebook :: [Cell] -> Metadata -> Notebook
notebook c m  = Notebook c m 4 2

-- | All cells have a set of source lines, and cell-level metadata
data CommonCellContent =
    CommonCellContent
    { source :: [String]
    , cellmetadata :: Metadata
    } deriving (Show, Generic, Eq)

type Metadata = Map.Map String Value

-- | A mime bundle
type MimeBundle = Map.Map String Value


-- | Streams are either Stdout or Stderr
data StreamName = Stdout | Stderr
    deriving (Show, Eq, Generic)

{- | A code cell can have multiple outputs - be they execute results, display data, or streams (stdout and stderr).

TODO: support output_type for display data and error.
 -}
data Output
    = ExecuteResult { data_ :: MimeBundle -- data is a haskell keyword
        , execution_count :: Int
        , outputmetadata :: Metadata
        }
    | Stream { name :: StreamName
        , text :: [String]
        }
    deriving (Show, Eq, Generic)

-- | Execution count can be unset (Nothing - serialized to undefined), or
-- just some integer
data ExecutionCount = ExecutionCount (Maybe Int)
   deriving (Show, Eq, Generic)

{- | Jupyter notebook cells come in three different flavors: Markdown,
 Code, and Raw. All three share a content structure with source (a list of
 strings) and metadata (a dictionary). In addition, the Code
 cells also have a list of Outputs, and an execution count.

 -}
data Cell
    = MarkdownCell CommonCellContent
    | CodeCell  CommonCellContent [Output] ExecutionCount
    | RawCell CommonCellContent
    deriving (Show, Generic, Eq)

