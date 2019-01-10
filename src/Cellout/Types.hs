{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
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

import GHC.Generics
import Data.Aeson (Value)
import qualified Data.Map.Strict as Map
import Data.Text

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

instance Semigroup Notebook where
    (Notebook c meta fmt fmt_minor) <> (Notebook c' meta' _ _) =
        Notebook (c ++ c') (meta<>meta') fmt fmt_minor
instance Monoid Notebook where
    mempty = Notebook mempty mempty 4 2

-- | convenience function for creating a Notebook with nbformat 4.2
notebook :: [Cell] -> Metadata -> Notebook
notebook c m  = Notebook c m 4 2

-- | All cells have a set of source lines, and cell-level metadata
data CommonCellContent =
    CommonCellContent
    { source :: [Text]
    , cellmetadata :: Metadata
    } deriving (Show, Generic, Eq)

-- TODO: how do I make instances of Applicative/Alternative/Monoid?
-- instance Alternative (CommonCellContent where
--     empty = CommonCellContent mempty mempty
-- hmm... I think I've got it, but am I abusing Monoid here?
instance Semigroup CommonCellContent where
    (CommonCellContent a b) <> (CommonCellContent x y) = CommonCellContent (a <> x) (b <> y)
instance Monoid CommonCellContent where
    mempty = CommonCellContent mempty mempty

type Metadata = Map.Map Text Value

-- | A mime bundle
type MimeBundle = Map.Map Text Value


-- | Streams are either Stdout or Stderr
data StreamName = Stdout | Stderr
    deriving (Show, Eq, Generic)

{- | A code cell can have multiple outputs - be they execute results, display data, or streams (stdout and stderr).
 -}
data Output
    = ExecuteResult { data_ :: Maybe MimeBundle -- data is a haskell keyword
        , execution_count :: Maybe Int -- NB3: added Maybe to data_ and execution_count
        , outputmetadata :: Metadata
        }
    | Stream { name :: StreamName
        , text :: [Text]
        }
    | DisplayData { data_ :: Maybe MimeBundle
        , outputmetadata :: Metadata
        }
    | ErrorData { ename :: Text
        , evalue :: Text
        , traceback :: [Text]
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

