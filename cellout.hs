
import Text.ParserCombinators.ReadP
import Data.List
import Data.Set (Set, empty)
import Control.Arrow -- for >>>

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
    | RawCell CommonCellContent
    deriving Show

-- let's think about this a bit, I'll be able to case-switch on cell type if I
-- go with the above, but is that something I will want to do? I guess it makes
-- the rest of the validation more explicit
--

testNb :: Notebook
testNb = Notebook "hallo.ipynb"
    [ MarkdownCell $ CommonCellContent ["yo", "I'm a multiline markdown cell"] empty
    , CodeCell $ CommonCellContent ["print ('hello')"] empty
    , CodeCell $ CommonCellContent [] empty
    , CodeCell $ CommonCellContent [""] empty
    , CodeCell $ CommonCellContent [""] empty
    , CodeCell $ CommonCellContent [""] empty
    , CodeCell $ CommonCellContent [""] empty
    , CodeCell $ CommonCellContent ["print ('goodbye')\n"] empty
    ]
    empty -- should I be using mempty here?

show' :: Cell -> String
show' cell =  case cell of
    MarkdownCell c -> foldMap markdown_indicator (source c)
    CodeCell c -> unlines $ source c

markdown_indicator :: String -> String
markdown_indicator x = "### " ++ x ++ "\n"

isMarkdown ::  Cell -> Bool
isMarkdown (MarkdownCell _) = True
isMarkdown _ = False

isCode ::  Cell -> Bool
isCode (CodeCell _) = True
isCode _ = False

-- TODO not 100% sure what empty cells actually show up as.
isEmpty ::  Cell -> Bool
isEmpty (MarkdownCell c) = source c == [""]
isEmpty (CodeCell c) = source c == [""]

---- let's do some quick filtering on cell type...
onlyMarkdown :: [Cell] -> [Cell]
onlyMarkdown = filter isMarkdown

onlyCode :: [Cell] -> [Cell]
onlyCode = filter isCode

clearEmpty :: [Cell] -> [Cell]
clearEmpty = filter (not . isEmpty)


mdBeforeCode :: Cell -> [Cell]
mdBeforeCode (CodeCell x) =
    [ MarkdownCell $  CommonCellContent [""] empty, (CodeCell x)]
mdBeforeCode x = [x]

mdBeforeEachCodeDumb :: [Cell] -> [Cell]
mdBeforeEachCodeDumb cells = concatMap mdBeforeCode cells

-- By keeping content's first argument as [Cells] -> [Cells], we allow both the
-- exclusion of cells, and the addition of new ones. Also, we can examin
-- adjacent cells to make decisions about what to add or remove.
--
-- TODO: This, then also suggests we should return a Notebook, instead of a string.
--
--}
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

onlyNonEmpty :: Notebook -> String
onlyNonEmpty
    = content clearEmpty

insertMd :: Notebook -> String
insertMd
    = content mdBeforeEachCodeDumb

main :: IO ()
main = do
    putStr (show testNb)
    putStrLn ""
    putStrLn $ printCells testNb
    putStrLn ""
    putStrLn $ onlyMarkdownContent testNb
    putStrLn "CODECODECODECODECODE"
    putStrLn $ onlyCodeContent testNb
    putStrLn "EMPTIES removed"
    putStrLn $ onlyNonEmpty testNb
    putStrLn "Extra MARKDOWN "
    putStrLn $ insertMd testNb




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

---- ARGH! why doesn't this work?
-- onlyCode2 :: [Cell] -> [Cell]
-- onlyCode2 = keep False True >>> filter
--  cellout.hs:58:33: error:
--      Ambiguous occurrence `filter'
--      It could refer to either `Data.List.filter',
--                               imported from `Data.List' at cellout.hs:3:1-16
--                               (and originally defined in `GHC.List')
--                            or `Data.Set.filter',
--                               imported from `Data.Set' at cellout.hs:4:1-15
--                               (and originally defined in `Data.Set.Internal')
--     |
--  58 | onlyCode2 = keep False True >>> filter
--     |                                 ^^^^^^


-- keep :: Bool -> Bool -> Cell -> Bool
-- keep md code x = case x of
--     MarkdownCell c -> md
--     CodeCell c -> code
--
-- keep :: Bool -> Bool -> Cell -> Bool
-- keep md code (MarkdownCell _)  = md
-- keep md code (CodeCell _)  = code
--
-- isMarkdown ::  Cell -> Bool
-- isMarkdown = keep True False
