
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
    [ MarkdownCell $ CommonCellContent ["yo"] empty
    , CodeCell $ CommonCellContent ["print ('hello')"] empty
    , CodeCell $ CommonCellContent ["print ('goodbye')"] empty
    ]
    empty

main :: IO ()
main = do
    putStr (show testNb)
