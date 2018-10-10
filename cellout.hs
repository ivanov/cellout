{-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE OverloadedStrings #-} -- would get rid of T.pack

import Control.Arrow -- for >>>
import Data.Aeson
import Data.List
import Data.Set (Set, empty)
import Data.Text.Encoding
import GHC.Generics
import Text.ParserCombinators.ReadP
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Lazy as HML
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

data Notebook =
    Notebook
    { cells :: [ Cell ]
    , nbmetadata :: Map.Map String String -- should be possible to do nested StringOrMap
    , nbformat :: Int
    , nbformat_minor :: Int
    } deriving (Show, Generic)

-- Notebook4 :: [Cell] -> (Map.Map String Sting)
notebook :: [Cell] -> Map.Map String String ->  Notebook
notebook c m  = Notebook c m 4 2

data CommonCellContent =
    CommonCellContent
    { source :: [String]
    , cellmetadata :: Map.Map String String -- same as nested comment above
    } deriving (Show, Generic)


type MimeBundle = Map.Map String String -- 'text' should get re-keyd to 'data' on serialization


-- TODO: output should be a list of mimebundles?
data Output
    = OutputExecutionResult
        { dat :: MimeBundle
        , execution_count :: Int
        , outputmetadata :: (Map.Map String String)
        }
    | OuputStream [String]
    deriving (Show, Eq, Generic)

data Cell
    = MarkdownCell CommonCellContent
    | CodeCell  CommonCellContent [Output]
    | RawCell CommonCellContent
    deriving (Show, Generic)


metaCorrector  :: String -> String
metaCorrector x =
    if isSuffixOf "metadata" x
    then "metadata"
    else x

dat2data :: String -> String
dat2data "dat" = "data"
dat2data x = x


instance FromJSON Notebook
instance ToJSON Notebook where
    toEncoding = genericToEncoding defaultOptions{ fieldLabelModifier = metaCorrector }


cell_type = T.pack "cell_type"
output = T.pack "outputs"

instance FromJSON Cell
instance ToJSON Cell where
    -- toJSON (MarkdownCell c) = object $ [ cell_type .= "markdown" ]
    -- toJSON (CodeCell c o) = object $ [ cell_type .= "code" ]
    toJSON (MarkdownCell c) = Object $ HML.insert cell_type (toJSON "markdown") (unobject $ toJSON c)
    toJSON (CodeCell c o) = merge (object [ cell_type .= "code", output .= o]) (toJSON c)

-- toJSONCellType :: String -> CommonCellContent -> Value
-- toJSONMarkdown ::
-- toJSONMarkdown =

unobject ::  Value -> HML.HashMap T.Text Value
unobject (Object x) =  x
unobject _ = HML.empty

merge :: Value -> Value -> Value
merge (Object x) (Object y) = Object $ HML.union x y

instance FromJSON CommonCellContent
instance ToJSON CommonCellContent where
    -- toEncoding = genericToEncoding defaultOptions{ fieldLabelModifier = metaCorrector }
    toEncoding = genericToEncoding defaultOptions{ fieldLabelModifier = metaCorrector }
    toJSON = genericToJSON defaultOptions{ fieldLabelModifier = metaCorrector }

instance FromJSON Output
instance ToJSON Output where
    toEncoding = genericToEncoding defaultOptions{
        sumEncoding = UntaggedValue,
        -- sumEncoding = ObjectWithSingleField,
        unwrapUnaryRecords = True,
        fieldLabelModifier = metaCorrector
        }
    toJSON = genericToJSON defaultOptions{
        sumEncoding = UntaggedValue,
        unwrapUnaryRecords = True,
        fieldLabelModifier = metaCorrector . dat2data,
        constructorTagModifier = \x -> "data"
        }



emptyOutput :: [Output]
emptyOutput = []

output1 :: String -> String -> Int -> [Output]
output1 k v i = [(OutputExecutionResult $ Map.singleton k v) i mempty]

-- let's think about this a bit, I'll be able to case-switch on cell type if I
-- go with the above, but is that something I will want to do? I guess it makes
-- the rest of the validation more explicit


testNb :: Notebook
testNb = notebook
    [ MarkdownCell $ CommonCellContent ["# In the Beginning"] mempty
    , MarkdownCell $ CommonCellContent ["yo", "I'm a multiline markdown cell"] mempty
    , MarkdownCell $ CommonCellContent ["yo", "I'm a multiline markdown cell"] mempty
    , CodeCell ( CommonCellContent ["print ('hello')"]  mempty) (output1 "text/plain" "hello" 1 )
    , CodeCell ( CommonCellContent [] mempty ) emptyOutput
    , CodeCell ( CommonCellContent [""] mempty ) emptyOutput
    , CodeCell ( CommonCellContent [""] mempty ) emptyOutput
    , CodeCell ( CommonCellContent [""] mempty ) emptyOutput
    , CodeCell ( CommonCellContent [""] mempty ) emptyOutput
    , CodeCell ( CommonCellContent ["print ('goodbye')\n"] mempty ) emptyOutput
    ]
    mempty -- should I be using mempty here?

oneNb = onlyCell 3 testNb
common :: Cell -> CommonCellContent
common (MarkdownCell c) = c
common (CodeCell c _) = c
common (RawCell c) = c


-- TODO: markdown_indicator will need to be language sensitve, passed in, or
-- else no makrdown cells should be included
asCode :: Cell -> String
asCode cell =  case cell of
    MarkdownCell c -> foldMap markdown_indicator (source c)
    CodeCell c o -> unlines $ source c


-- TODO: same as markdown_indicator above, we need a code_indicator below for
-- language specific syntax highlighting for the people who want that sort of
-- thing
asMarkdown:: Cell -> String
asMarkdown cell =  case cell of
    MarkdownCell c -> unlines ( source c) ++ "\n"
    -- -- At some point I thought I might need to add extra new lines between
    -- -- markdown, but I dont' think that' s true...
    -- MarkdownCell c -> unlines . (intersperse "\n") $ source c
    CodeCell c o -> "```\n" ++ (unlines $ source c) ++ "```\n\n"

markdown_indicator :: String -> String
markdown_indicator x = "### " ++ x ++ "\n"

isMarkdown ::  Cell -> Bool
isMarkdown (MarkdownCell _) = True
isMarkdown _ = False

isCode ::  Cell -> Bool
isCode (CodeCell _ _) = True
isCode _ = False

-- TODO not 100% sure what empty cells actually show up as.
isEmpty ::  Cell -> Bool
isEmpty (MarkdownCell c) = source c == [""]
isEmpty (CodeCell c o) = source c == [""] && o == emptyOutput

---- let's do some quick filtering on cell type...
onlyMarkdown :: [Cell] -> [Cell]
onlyMarkdown = filter isMarkdown

onlyCode :: [Cell] -> [Cell]
onlyCode = filter isCode

clearEmpty :: [Cell] -> [Cell]
clearEmpty = filter (not . isEmpty)

clearMetadata :: Cell -> Cell
clearMetadata (MarkdownCell (CommonCellContent src _)) = MarkdownCell (CommonCellContent src mempty)
clearMetadata (CodeCell (CommonCellContent src _) o) = CodeCell (CommonCellContent src mempty) o
clearMetadata (RawCell (CommonCellContent src _)) = RawCell (CommonCellContent src mempty)

clearCellMetadata :: [Cell] -> [Cell]
clearCellMetadata = fmap clearMetadata

clearOutput :: Cell -> Cell
clearOutput (CodeCell (CommonCellContent src md) _) = CodeCell (CommonCellContent src md) emptyOutput


mdBeforeCode :: Cell -> [Cell]
mdBeforeCode (CodeCell x o) =
    [ MarkdownCell $  CommonCellContent [""] mempty, (CodeCell x o)]
mdBeforeCode x = [x]

-- Inserting more cells
mdBeforeEachCodeDumb :: [Cell] -> [Cell]
mdBeforeEachCodeDumb cells = concatMap mdBeforeCode cells

-- By keeping content's first argument as [Cells] -> [Cells], we allow both the
-- exclusion of cells, and the addition of new ones. Also, we can examin
-- adjacent cells to make decisions about what to add or remove.
--
-- TODO: This, then also suggests we should return a Notebook, instead of a string.
--
--}
contentFiltering :: ([Cell] -> [Cell]) -> Notebook  -> String
contentFiltering f
    = printCells . cellsFilter f

cellMap :: (Cell -> a) -> Notebook  -> [a]
cellMap f n = map f (cells n)

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

printCells :: Notebook -> String
printCells
    = cells
    >>> fmap asCode
    >>> concat

-- for now, this ignores any metadata...
showNb :: (Cell -> String) -> Notebook -> String
showNb f = cells
    >>> fmap f
    >>> concat

onlyMarkdownContent :: Notebook -> String
onlyMarkdownContent
    = contentFiltering onlyMarkdown

onlyCodeContent :: Notebook -> String
onlyCodeContent
    = contentFiltering onlyCode

onlyNonEmpty :: Notebook -> Notebook
onlyNonEmpty
    = cellsFilter clearEmpty

insertMd :: Notebook -> String
insertMd
    = contentFiltering mdBeforeEachCodeDumb

reversed :: Notebook -> String
reversed
    = contentFiltering reverse

onlyCell ::  Int -> Notebook -> Notebook
onlyCell i (Notebook c n f m) =  Notebook [ c !! i ] n f m

source' :: Cell -> [String]
source' (MarkdownCell c) = source c
source' (CodeCell c _) = source c

wordCount :: Cell -> (Int, Int, Int)
wordCount c = let s =  unlines . source' $  c
  in
    (length (lines s), length (words s), length s)

writeNb :: FilePath -> Notebook -> IO ()
writeNb file nb = LB.writeFile file (encode nb)

main :: IO ()
main = do
    -- putStr (show testNb)
    -- putStrLn "%%% PRINT CELLS"
    -- putStrLn $ printCells testNb
    -- putStrLn $ showNb asCode testNb
    -- putStrLn $ showNb asMarkdown (onlyNonEmpty testNb)
    -- putStrLn $ T.unpack . decodeUtf8 . LB.toStrict . encode $ (onlyNonEmpty testNb)
    let newNb = (onlyCell 3 testNb)
        in writeNb "C:\\bbg\\jlabremix\\tmp\\hi.ipynb"  newNb
    --(toEncoding . source . common . (!! 3) . cells) testNb




-- ALTERNATIVES
--
---printCells :: Notebook -> String
-- printCells nb
--     = concat (fmap asCode $ cells nb )
--
-- onlyMarkdownContent :: Notebook -> String
-- onlyMarkdownContent nb = unwords . fmap asCode $ onlyMarkdown $ cells (nb)

-- printCells :: Notebook -> String
-- printCells
--     = cells
--     >>> fmap asCode
--     >>> concat

---- ARGH! why doesn't this work?
-- -- 2018-10-09 - I know now, you have to qualified import
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
--
-- 2018-10-02
-- ideas:
-- [ ] look at some nbconvert stuff for functionality
-- [ ] executable traversal (writing back to the code cell output)
-- [ ] command-line spellchecking facility?
-- [ ] wrapped (kanten-style) notebook presentation?
-- [ ] interactive mode with live preview  for size and content?
-- [ ] metadata editor
-- [ ] selecting only cells matching a tag, or filtering them out
-- [ ] interactive cell-level editing marking/tagging
-- [ ] nbformat fuzzing tool?
--
-- output as ->
-- [x] executable script
-- [x] code only (filter codecell and strip output)
-- [x] markdown
-- [ ] notebook
--
-- 2018-10-09
-- [ ] current serialization doesn't match nbformat:
--      Unreadable Notebook: C:\bbg\jlabremix\tmp\hi.ipynb AttributeError('cell_type',)
-- [ ] when you have more than one execute_count output in a code cell, which one should be shown? all?
-- [ ] Unreadable Notebook: C:\bbg\jlabremix\tmp\hi.ipynb UnboundLocalError("local variable 'newcell' referenced before assignment",)
-- [ ] probably remove the nbformat major/minor from the Notebook model and
--     have some mixin that does that at the end (most filters won't car about nbformat version)
--
-- from Anthony:
--     multiparameter type classes --
--     threadscope...
-- typeclasses - Show Eq -- Functor Traversable Applicative Monoid Monad
-- import Data.List hiding filter  -- from Anthony
