
import Text.ParserCombinators.ReadP
import Data.List
import Data.Set
import Control.Arrow

data Notebook =
    Notebook
    { filename :: String
    , cells :: [ Cell ]
    , nbmetadata :: Set String
    } deriving Show

data CommonCellContent =
    CommonCellContent
    { source :: [String]
    , metadata :: Set String
    } deriving Show

data Cell
    = MarkdownCell CommonCellContent
    | CodeCell  CommonCellContent
    -- | RawCell CellContent
    deriving Show

-- let's think about this a bit, I'll be able to case-switch on cell type if I
-- go with the above, but is that something I will want to do? I guess it makes
-- the rest of the validation more explicit
--

testNb :: Notebook
testNb = Notebook "hallo.ipynb"
    [ MarkdownCell $ CommonCellContent ["yo", "I'm a multiline markdown cell"] empty
    , CodeCell $ CommonCellContent ["print ('hello')"] empty
    , CodeCell $ CommonCellContent ["print ('goodbye')\n"] empty
    ]
    empty

show' :: Cell -> String
show' cell =  case cell of
    MarkdownCell c -> foldMap (\x -> "### " ++ x ++ "\n") (source c)
    CodeCell c -> unlines $ source c


keep :: Bool -> Bool -> Cell -> Bool
keep md code x =  case x of
    MarkdownCell c -> md
    CodeCell c -> code

-- let's do some quick filtering on cell type...
onlyMarkdown :: [Cell] -> [Cell]
onlyMarkdown = Data.List.filter $ keep True False

onlyCode :: [Cell] -> [Cell]
onlyCode = Data.List.filter $ keep False True

{- By keeping content's first argument as [Cells] -> [Cells], we allow both the
 - exclusion of cells, and the addition of new ones.
 -
 - TODO: This, then also suggests we should return a Notebook, instead of a string.
 -
 -}
content :: ([Cell] -> [Cell]) -> Notebook  -> String
content filter
    = cells
    >>> filter
    >>> fmap show'
    >>> concat

printCells :: Notebook -> String
printCells
    = content id

onlyMarkdownContent :: Notebook -> String
onlyMarkdownContent
    = content onlyMarkdown

onlyCodeContent :: Notebook -> String
onlyCodeContent
    = content onlyCode

main :: IO ()
main = do
    putStr (show testNb)
    putStr "\n"
    putStr $ printCells testNb
    putStr "\n"
    putStr $ onlyMarkdownContent testNb
    putStr "CODECODECODECODECODE \n"
    putStr $ onlyCodeContent testNb




-- ALTERNATIVES
--
---printCells :: Notebook -> String
-- printCells nb
--     = concat (fmap show' $ cells nb )
--
-- onlyMarkdownContent :: Notebook -> String
-- onlyMarkdownContent nb = unwords . fmap show' $ onlyMarkdown $ cells (nb)

-- printCells :: Notebook -> String
-- printCells
--     = cells
--     >>> fmap show'
--     >>> concat

