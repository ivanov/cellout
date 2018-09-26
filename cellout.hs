
import Text.ParserCombinators.ReadP
import Data.List

data Notebook = 
    Notebook 
    { filename :: String
    , cells :: [ Cell ]
    } deriving Show

data Cell = MarkdownCell | CodeCell | RawCell
    deriving Show


testNb :: Notebook
testNb = Notebook "hallo.ipynb" [MarkdownCell, CodeCell, CodeCell]
main :: IO ()
main = do
    putStr (show testNb)
