{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
module Cellout
    ( Notebook(..)
    , Cell(..)
    , CommonCellContent(..)
    , Output(..)
    , ExecutionCount(..)
    , MimeBundle
    , StreamName(..)
    , Metadata
    , notebook
    , emptyOutput
    , testNb
    , trivialNb
    , common
    , asCode
    , asMarkdown
    , isMarkdown
    , isCode
    , isEmpty
    , onlyMarkdown
    , onlyCode
    , clearEmpty
    , clearMetadata
    , clearCellMetadata
    , clearOutput
    , clearOutputs
    , clearPrompt
    , mdBeforeCode
    , mdBeforeEachCodeDumb
    , cellMap
    , cellsFilter
    , nbFilter
    , clearNbMetadata
    , printCells
    , showNb
    , onlyMarkdownContent
    , onlyCodeContent
    , onlyNonEmpty
    , reversed
    , onlyCell
    , wordCount
    , readNb
    , readNb'
    , writeNb
    , collectInfo
    , stripOutputIO
    , nbAsJSONString
    ) where

import Control.Arrow -- for >>>
import Data.Aeson (eitherDecode, encode, toJSON)
import Data.Text.Encoding
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import System.Environment (getArgs)

import Cellout.Types
import Cellout.JSON

-- | convenience function for creating a Notebook with nbformat 4.2
notebook :: [Cell] -> Metadata -> Notebook
notebook c m  = Notebook c m 4 2

empty_execution_count = ExecutionCount Nothing


-- | A convenience function for empty output
emptyOutput :: [Output]
emptyOutput = []

output1 :: T.Text -> [String] -> Int -> Output
output1 k v i = ExecuteResult (Map.singleton k (toJSON v)) i mempty


-- | A multi-cell example notebook (useful for testing and ghci work)
testNb :: Notebook
testNb = notebook
    [ MarkdownCell $ CommonCellContent ["# In the Beginning\n"] mempty
    , RawCell $ CommonCellContent ["yo\n", "I'm a multiline raw cell\n"] mempty
    , MarkdownCell $ CommonCellContent ["yo\n", "I'm a multiline markdown cell\n"] mempty
    , CodeCell ( CommonCellContent ["print ('hello')\n", "'yo there'"]  mempty) [Stream Stdout ["hello\n"], output1 "text/plain" ["'yo there'"] 2 ] (ExecutionCount (Just 2))
    , CodeCell ( CommonCellContent mempty mempty) emptyOutput empty_execution_count
    , CodeCell ( CommonCellContent mempty mempty) [Stream Stdout ["yo output\n"]]  (ExecutionCount (Just 3))
    , CodeCell ( CommonCellContent mempty mempty) emptyOutput empty_execution_count
    , CodeCell ( CommonCellContent mempty mempty) emptyOutput empty_execution_count
    , CodeCell ( CommonCellContent mempty mempty) emptyOutput empty_execution_count
    , CodeCell ( CommonCellContent ["print ('goodbye')\n"] mempty) emptyOutput empty_execution_count
    ]
    mempty -- should I be using mempty here?

-- | A notebook with no cells and no metadata
trivialNb :: Notebook
trivialNb = notebook mempty mempty

oneNb = onlyCell 3 testNb

-- | returns just the CommonCellContent for any type of cell
common :: Cell -> CommonCellContent
common (MarkdownCell c) = c
common (CodeCell c _ _) = c
common (RawCell c) = c


{- Converts code cells to strings in a trivial manner, but pre-pends the
markdown_indicator in front of each line for Markdown and Raw cells

TODO: markdown_indicator will need to be language sensitve, passed in, or
else no markdown cells should be included.
-}
asCode :: Cell -> T.Text
asCode cell =  case cell of
    CodeCell c _ _-> T.unlines $ source c
    MarkdownCell c -> foldMap markdown_indicator (source c)
    RawCell c      -> foldMap markdown_indicator (source c)


-- TODO: same as markdown_indicator above, we need a code_indicator below for
-- language specific syntax highlighting for the people who want that sort of
-- thing
asMarkdown :: Cell -> T.Text
asMarkdown cell =  case cell of
    MarkdownCell c -> T.concat [ T.unlines ( source c) , T.pack "\n"]
    -- -- At some point I thought I might need to add extra new lines between
    -- -- markdown, but I dont' think that' s true...
    -- MarkdownCell c -> T.unlines . (intersperse "\n") $ source c
    CodeCell c o _ -> T.concat ["```\n", (T.unlines $ source c), "```\n\n"]

markdown_indicator :: T.Text -> T.Text
markdown_indicator x = T.concat ["### ", x , "\n"]

isMarkdown ::  Cell -> Bool
isMarkdown (MarkdownCell _) = True
isMarkdown _ = False

isCode ::  Cell -> Bool
isCode (CodeCell _ _ _) = True
isCode _ = False

-- TODO not 100% sure what empty cells actually show up as.
isEmpty ::  Cell -> Bool
isEmpty (MarkdownCell c) = source c == []
isEmpty (CodeCell c o _) = source c == [] && o == emptyOutput

---- let's do some quick filtering on cell type...
onlyMarkdown :: [Cell] -> [Cell]
onlyMarkdown = filter isMarkdown

onlyCode :: [Cell] -> [Cell]
onlyCode = filter isCode

clearEmpty :: [Cell] -> [Cell]
clearEmpty = filter (not . isEmpty)

clearMetadata :: Cell -> Cell
clearMetadata (MarkdownCell (CommonCellContent src _)) = MarkdownCell (CommonCellContent src mempty)
clearMetadata (CodeCell (CommonCellContent src _) o i) = CodeCell (CommonCellContent src mempty) o i
clearMetadata (RawCell (CommonCellContent src _)) = RawCell (CommonCellContent src mempty)

clearCellMetadata :: [Cell] -> [Cell]
clearCellMetadata = fmap clearMetadata

{- | Removes output from a code cell, all other cells returned unmodified.

NB: clearOutput clears both the output and resets the execution count
-}
clearOutput :: Cell -> Cell
clearOutput (CodeCell (CommonCellContent src md) _ _) = CodeCell (CommonCellContent src md) emptyOutput empty_execution_count
clearOutput x = x

-- | Removes output from all code cells in the notebook.
clearOutputs :: Notebook -> Notebook
clearOutputs =  cellsFilter (fmap clearOutput)

{- | When passed a code cell, returns a copy with a cleared execution count cleared. For all other cell types, does nothing and returns the original.
NB: clearOutput clears both the output and resets the execution count
-}
clearPrompt :: Cell -> Cell
clearPrompt (CodeCell (CommonCellContent src md) o _) = CodeCell (CommonCellContent src md) o empty_execution_count
clearPrompt x = x

-- | Removes prompts from all code cells in the notebook.
clearPrompts :: Notebook -> Notebook
clearPrompts =  cellsFilter (fmap clearPrompt)

{- | Returns a list of of one or two cells, depending on the input cell.
If the input cell is a code cell, you get back a markdown cell, followed by
the original, otherwise, a list of just the one original cell is returned.
-}
mdBeforeCode :: Cell -> [Cell]
mdBeforeCode cell@(CodeCell x o i) =
    [ MarkdownCell $  CommonCellContent [""] mempty, cell ]
-- don't be scared, cell@(...) is just an alias for (CodeCell x o i)
mdBeforeCode x = [x]

-- | Inserts an empty Markdown cell before every code cell
mdBeforeEachCodeDumb :: [Cell] -> [Cell]
mdBeforeEachCodeDumb cells = concatMap mdBeforeCode cells


{- | Collect arbitrary information from each cell in a notebook and returns that list.
 -}
cellMap :: (Cell -> a) -> Notebook  -> [a]
cellMap f n = map f (cells n)

{- | Per-cell modification for the notebook. Useful for
 -}
cellFilter :: (Cell -> Cell) -> Notebook  -> Notebook
-- cellFilter f nb@(Notebook cs nbmeta fmt m)
--     = Notebook (cellMap f nb) nbmeta fmt m
cellFilter f = cellsFilter (fmap f)

{- | A generic filter for the list of cells in a notebook.

 By keeping content's first argument as [Cells] -> [Cells], we allow both the
 exclusion of cells, and the addition of new ones, on top of any tranformation
 that take place within the cell itself. Also, we can examine adjacent cells to
 make decisions about what to add or remove.

See also: 'cellFilter'
 -}
-- How do I copy over most elements from the old notebook and just change the cells aspect of it?
cellsFilter :: ([Cell] -> [Cell]) -> Notebook  -> Notebook
cellsFilter f (Notebook cs nbmeta fmt m)
    = Notebook (f cs) nbmeta fmt m

-- oh, well, this is kind of dumb, because this is just
--  function application... but at least it makes more explicit
--  what kind of transformations we can have here (a richer
--  set)
nbFilter :: (Notebook -> Notebook) -> Notebook  -> Notebook
nbFilter f = f

clearNbMetadata :: Notebook -> Notebook
clearNbMetadata (Notebook cs nbmeta f m) = Notebook cs mempty f m

printCells :: Notebook -> T.Text
printCells
    = cells
    >>> fmap asCode
    >>> T.concat
-- the above is equiavalent to
-- = concat (fmap asCode $ cells nb )

-- for now, this ignores any metadata...
showNb :: (Cell -> T.Text) -> Notebook -> T.Text
showNb f = cells
    >>> fmap f
    >>> T.concat

onlyMarkdownContent :: Notebook -> Notebook
onlyMarkdownContent
    = cellsFilter onlyMarkdown

onlyCodeContent :: Notebook -> Notebook
onlyCodeContent
    = cellsFilter onlyCode

onlyNonEmpty :: Notebook -> Notebook
onlyNonEmpty
    = cellsFilter clearEmpty

insertMd :: Notebook -> Notebook
insertMd
    = cellsFilter mdBeforeEachCodeDumb

reversed :: Notebook -> Notebook
reversed
    = cellsFilter reverse

{- | Return a notebook containing only the cell at position `i`
 -}
onlyCell ::  Int -> Notebook -> Notebook
onlyCell i (Notebook c n f m) =  Notebook [ c !! i ] n f m

source' :: Cell -> [T.Text]
source' (MarkdownCell c) = source c
source' (CodeCell c _ _) = source c

wordCount :: Cell -> (Int, Int, Int)
wordCount c = let s =  T.unlines . source' $  c
  in
    (length (T.lines s), length (T.words s), T.length s)

tupleSum :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
tupleSum (a, b, c) (x, y, z) = (a+x, b+y, c+z)

cellCount :: Cell -> (Int, Int, Int) -> (Int, Int, Int)
cellCount (CodeCell _ _ _) (x, y, z) = (x+1, y, z)
cellCount (MarkdownCell _) (x, y, z) = (x, y+1, z)
cellCount (RawCell _) (x, y, z) = (x, y, z+1)

{- | Returns a tuple of (Code, Markdow, Raw) cells counts -}
countCellsByType :: [Cell] -> (Int, Int, Int)
countCellsByType cells = foldr cellCount (0,0,0) cells

plural :: Int -> String
plural 1 = ""
plural _ = "s"

{- | Get a summary string for this notebook (cell breakdown by type).

TODO: generalize this, don't just write to a string immediately.
 -}
collectInfo :: Notebook -> String
collectInfo nb
    = let
        (code, md, raw) = countCellsByType (cells nb)
        sum = code + md + raw
    in
        show sum ++ " cell" ++ plural sum ++ " (" ++ show code ++ " code, " ++ show md ++ " markdown)"

stripOutputIO :: String -> String -> IO ()
stripOutputIO inputFile outputFile = do
    input <- LB.readFile inputFile
    case (eitherDecode input) :: (Either String Notebook) of
        Left err -> putStrLn err
        Right nb -> writeNb outputFile nb


