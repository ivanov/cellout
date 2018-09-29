
import Text.ParserCombinators.ReadP
import Data.List
import Data.Set

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
    , CodeCell $ CommonCellContent ["print ('goodbye')"] empty
    ]
    empty

-- Man, so how do I unwrap the union data type into its constituent parts?
show' :: Cell -> String
show' cell =  case cell of
            --MarkdownCell c -> unlines $ "### " ++ (source c)
            MarkdownCell c -> foldMap (\x -> "### " ++ x ++ "\n") (source c)
            CodeCell c -> unlines $ source c

printCells :: Notebook -> String
printCells nb = unwords (fmap show' $ cells nb )


-- let's do some quick 
main :: IO ()
main = do
    putStr (show testNb)
    putStr "\n"
    putStr $ printCells testNb
