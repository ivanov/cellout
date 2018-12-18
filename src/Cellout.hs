{-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE OverloadedStrings #-} -- would get rid of T.pack
module Cellout
    ( Notebook
    , Cell
    , notebook
    , metaCorrector
    , dataKeywordFix
    , unobject
    , merge
    , emptyOutput
    , output1
    , testNb
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
    , contentFiltering
    , cellMap
    , cellsFilter
    , nbFilter
    , clearNbMetadata
    , printCells
    , showNb
    , onlyMarkdownContent
    , onlyCodeContent
    , onlyNonEmpty
    , insertMd
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
import Data.Aeson
import Data.Aeson.Encode.Pretty
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

import System.Environment (getArgs)

data Notebook =
    Notebook
    { cells :: [ Cell ]
    , nbmetadata :: Map.Map String Value -- should be possible to do nested StringOrMap
    , nbformat :: Int
    , nbformat_minor :: Int
    } deriving (Show, Generic, Eq)

-- Notebook4 :: [Cell] -> (Map.Map String Sting)
notebook :: [Cell] -> Map.Map String Value ->  Notebook
notebook c m  = Notebook c m 4 2

data CommonCellContent =
    CommonCellContent
    { source :: [String]
    , cellmetadata :: Map.Map String String -- same as nested comment above
    } deriving (Show, Generic, Eq)


type MimeBundle = Map.Map String [String] -- 'text' should get re-keyd to 'data' on serialization


toLower = (T.unpack . T.toLower . T.pack)

data StdSomething = Stdout | Stderr
    deriving (Show, Eq, Generic)

instance ToJSON StdSomething where
    toJSON = genericToJSON defaultOptions{
        constructorTagModifier = toLower
    }

instance FromJSON StdSomething where
    parseJSON = genericParseJSON defaultOptions{
        constructorTagModifier = toLower
    }

-- TODO: output should be a list of mimebundles?
-- TODO: we take advantage of the default TaggedObject JSON-ization of records
-- into the object to minimize
data Output
    = ExecuteResult { data_ :: MimeBundle -- data is a haskell keyword
        , execution_count :: Int
        , outputmetadata :: (Map.Map String String)
        }
    | Stream { name :: StdSomething
        , text :: [String]
        }
    deriving (Show, Eq, Generic)

data ExecutionCount = ExecutionCount (Maybe Int)
   deriving (Show, Eq, Generic)

data Cell
    = MarkdownCell CommonCellContent
    | CodeCell  CommonCellContent [Output] ExecutionCount
    | RawCell CommonCellContent
    deriving (Show, Generic, Eq)


metaCorrector  :: String -> String
metaCorrector x =
    if isSuffixOf "metadata" x
    then "metadata"
    else x

dataKeywordFix :: String -> String
dataKeywordFix "data_" = "data"
dataKeywordFix x = x

instance FromJSON Notebook where
    parseJSON = genericParseJSON  defaultOptions{ fieldLabelModifier = metaCorrector }
instance ToJSON Notebook where
    toEncoding = genericToEncoding defaultOptions{ fieldLabelModifier = metaCorrector }
    toJSON = genericToJSON defaultOptions{ fieldLabelModifier = metaCorrector }


cell_type = T.pack "cell_type"
outputs = T.pack "outputs"

empty_execution_count = ExecutionCount Nothing

--instance FromJSON Cell --where
instance FromJSON Cell where
    --  parseJSON = genericParseJSON defaultOptions  {
    --    }
    parseJSON (Object v) =  do
        cell_type <- v .: T.pack "cell_type"
        case cell_type of
            "markdown" -> MarkdownCell <$> (parseJSON (Object v) )
            "code" -> CodeCell <$> (parseJSON (Object v) ) <*>  (v .: T.pack "outputs") <*>  parseJSON (Object v)
            _ ->  fail "unrecognized cell type"
            -- "code" -> CodeCell <$> (parseJSON (Object v) ) <*>  (parseJSON (v .: T.pack "outputs")) <*>  ((v .: T.pack "execution_count"))

instance ToJSON Cell where
    -- toJSON (MarkdownCell c) = object $ [ cell_type .= "markdown" ]
    -- toJSON (CodeCell c o i) = object $ [ cell_type .= "code" ]
    toJSON (MarkdownCell c) = Object $ HML.insert cell_type (toJSON "markdown") (unobject $ toJSON c)
    -- TODO: change this to genericToEncoding with new options
    -- TODO: I should be able to use <> to concatenate the hashmaps here, no?
    toJSON (CodeCell c o i) = merge (object [ cell_type .= "code", outputs .= o])
                            $ merge (toJSON c) (toJSON i)

-- toJSONCellType :: String -> CommonCellContent -> Value
-- toJSONMarkdown ::
-- toJSONMarkdown =

unobject ::  Value -> HML.HashMap T.Text Value
unobject (Object x) =  x
unobject _ = HML.empty

merge :: Value -> Value -> Value
merge (Object x) (Object y) = Object $ HML.union x y

instance FromJSON CommonCellContent where
    parseJSON = genericParseJSON  defaultOptions{ fieldLabelModifier = metaCorrector }
instance ToJSON CommonCellContent where
    -- toEncoding = genericToEncoding defaultOptions{ fieldLabelModifier = metaCorrector }
    toEncoding = genericToEncoding defaultOptions{ fieldLabelModifier = metaCorrector }
    toJSON = genericToJSON defaultOptions{ fieldLabelModifier = metaCorrector }

instance FromJSON Output where
    parseJSON = genericParseJSON defaultOptions{
        sumEncoding = TaggedObject "output_type" "",
        -- I want taggedflattened object, but instead rewrote  Output to have records
        unwrapUnaryRecords = True,
        fieldLabelModifier = metaCorrector . dataKeywordFix,
        constructorTagModifier = \x -> if x == "ExecuteResult" then "execute_result" else (toLower x)
        }
    --     sumEncoding = TaggedObject "output_type" "contents",
    --     unwrapUnaryRecords = True,
    --     fieldLabelModifier = metaCorrector . dataKeywordFix
    --     constructorTagModifier = \x -> if x == "OutputExecuteResult" then "execute_result" else x
    -- }
    --parseJSON (Object v) = do
    --    output_type <- v .:  T.pack "output_type"
    --    case output_type of
    --        "stream" -> do
    --            name_ <- v .: T.pack "name"
    --            case name of
    --                "stdout" -> do
    --                    text <- (v .: T.pack "text")
    --                    Stream Stdout text
    --        -- "execute_result" -> ExecuteResult <$> parseJSON (Object v)

instance ToJSON Output where
    toJSON = genericToJSON defaultOptions{
        -- sumEncoding = UntaggedValue,
        sumEncoding = TaggedObject "output_type" "",
        -- I want taggedflattened object, but instead rewrote  Output to have records
        unwrapUnaryRecords = True,
        fieldLabelModifier = metaCorrector . dataKeywordFix,
        constructorTagModifier = \x -> if x == "ExecuteResult" then "execute_result" else (toLower x)
        }


instance FromJSON ExecutionCount where
    -- parseJSON = genericParseJSON defaultOptions{
    --     sumEncoding = ObjectWithSingleField,
    --     tagSingleConstructors = True,
    --     constructorTagModifier = \x -> if x == "execution_count" then "ExecutionCount" else x
    -- }
    -- -- ARGH, so frustrating that I can't figure out how to derive this and keep things generic
    parseJSON (Object v) = ExecutionCount <$>  v .: T.pack "execution_count"
    --parseJSON (Number n) = ExecutionCount <$> (Just n)
    --parseJSON (Number n) =  ExecutionCount <$> (Just n)
    --parseJSON (Number n) =  ExecutionCount <$> (Just toInteger(n))
    --parseJSON (Number x) = fail ("No idea what this is: " ++ show(toBoundedInteger (Number x)))

instance ToJSON ExecutionCount where
    toEncoding = genericToEncoding defaultOptions{
        sumEncoding = ObjectWithSingleField,
        tagSingleConstructors = True,
        constructorTagModifier = \x -> if x == "ExecutionCount" then "execution_count" else x
        }
    toJSON = genericToJSON defaultOptions{
        sumEncoding = ObjectWithSingleField,
        tagSingleConstructors = True,
        constructorTagModifier = \x -> if x == "ExecutionCount" then "execution_count" else x
        }


ecj :: Value
ecj = toJSON (ExecutionCount (Just 1))

ecn :: Value
ecn = toJSON (ExecutionCount Nothing)

ec :: Result ExecutionCount
ec = fromJSON  ecj

ec2 :: Result ExecutionCount
ec2 = fromJSON  ecn

-- map

emptyOutput :: [Output]
emptyOutput = []

output1 :: String -> [String] -> Int -> Output
output1 k v i = ExecuteResult (Map.singleton k v) i mempty

-- let's think about this a bit, I'll be able to case-switch on cell type if I
-- go with the above, but is that something I will want to do? I guess it makes
-- the rest of the validation more explicit


testNb :: Notebook
testNb = notebook
    [ MarkdownCell $ CommonCellContent ["# In the Beginning\n"] mempty
    , MarkdownCell $ CommonCellContent ["yo\n", "I'm a multiline markdown cell\n"] mempty
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

trivialNb :: Notebook
trivialNb = notebook mempty mempty

roundtrip :: Notebook -> Result Notebook
roundtrip = fromJSON . toJSON

oneNb = onlyCell 3 testNb

common :: Cell -> CommonCellContent
common (MarkdownCell c) = c
common (CodeCell c _ _) = c
common (RawCell c) = c


-- TODO: markdown_indicator will need to be language sensitve, passed in, or
-- else no makrdown cells should be included
asCode :: Cell -> String
asCode cell =  case cell of
    MarkdownCell c -> foldMap markdown_indicator (source c)
    CodeCell c _ _-> unlines $ source c


-- TODO: same as markdown_indicator above, we need a code_indicator below for
-- language specific syntax highlighting for the people who want that sort of
-- thing
asMarkdown :: Cell -> String
asMarkdown cell =  case cell of
    MarkdownCell c -> unlines ( source c) ++ "\n"
    -- -- At some point I thought I might need to add extra new lines between
    -- -- markdown, but I dont' think that' s true...
    -- MarkdownCell c -> unlines . (intersperse "\n") $ source c
    CodeCell c o _ -> "```\n" ++ (unlines $ source c) ++ "```\n\n"

markdown_indicator :: String -> String
markdown_indicator x = "### " ++ x ++ "\n"

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

{- NB: clearOutput clears both the output and resets the execution count -}
clearOutput :: Cell -> Cell
clearOutput (CodeCell (CommonCellContent src md) _ _) = CodeCell (CommonCellContent src md) emptyOutput empty_execution_count
clearOutput x = x

clearOutputs :: Notebook -> Notebook
clearOutputs =  cellsFilter (fmap clearOutput)

{- NB: clearOutput clears both the output and resets the execution count -}
clearPrompt :: Cell -> Cell
clearPrompt (CodeCell (CommonCellContent src md) o _) = CodeCell (CommonCellContent src md) o empty_execution_count
clearPrompt x = x

mdBeforeCode :: Cell -> [Cell]
mdBeforeCode (CodeCell x o i) =
    [ MarkdownCell $  CommonCellContent [""] mempty, (CodeCell x o i)]
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

{- Return a notebook containing only the cell at position `i`
 -}
onlyCell ::  Int -> Notebook -> Notebook
onlyCell i (Notebook c n f m) =  Notebook [ c !! i ] n f m

source' :: Cell -> [String]
source' (MarkdownCell c) = source c
source' (CodeCell c _ _) = source c

wordCount :: Cell -> (Int, Int, Int)
wordCount c = let s =  unlines . source' $  c
  in
    (length (lines s), length (words s), length s)

encode' :: ToJSON a => a -> LB.ByteString
encode' = encodePretty' defConfig{confIndent=Spaces 1, confCompare=compare}

writeNb :: FilePath -> Notebook -> IO ()
writeNb file nb = LB.writeFile file (encode' nb)

readNb :: FilePath -> IO (Either String Notebook)
readNb f = do
    input <- LB.readFile f
    return (eitherDecode input)

readNb' :: FilePath -> IO (Maybe Notebook)
readNb' f = do
    input <- LB.readFile f
    return (decode input)

tupleSum :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
tupleSum (a, b, c) (x, y, z) = (a+x, b+y, c+z)

{- returns a tuple of (Code, Markdow, Raw) cells counts -}
cellCount :: Cell -> (Int, Int, Int) -> (Int, Int, Int)
cellCount (CodeCell _ _ _) (x, y, z) = (x+1, y, z)
cellCount (MarkdownCell _) (x, y, z) = (x, y+1, z)
cellCount (RawCell _) (x, y, z) = (x, y, z+1)

{- returns a tuple of (Code, Markdow, Raw) cells counts -}
countCellsByType :: [Cell] -> (Int, Int, Int)
countCellsByType cells = foldr cellCount (0,0,0) cells

collectInfo :: Notebook -> String
collectInfo nb = let (code, md, raw) = countCellsByType (cells nb)
    in
        show (code + md + raw) ++ " cells, (" ++ show code ++ " code, " ++ show md ++ " markdown)"

stripOutputIO :: String -> String -> IO ()
stripOutputIO inputFile outputFile = do
    input <- LB.readFile inputFile
    --LB.writeFile outputFile $ encode testNb
    case (eitherDecode input) :: (Either String Notebook) of
        Left err -> putStrLn err
        Right nb -> writeNb outputFile nb
            --
            -- writeFile outputFile ( (T.unpack . decodeUtf8 . LB.toStrict . encode . clearOutputs) testNb)
            -- LB.writeFile outputFile $ ( encode . clearOutputs ) testNb
            --LB.writeFile outputFile $ (encode)  nb
            -- putStrLn $ decodeUtf8 . LB.unpack. encode $ (onlyNonEmpty testNb)

nbAsJSONString :: Notebook -> String
nbAsJSONString = T.unpack . decodeUtf8 . LB.toStrict . encode

main :: IO ()
main = do
    -- (toEncoding . source . common . (!! 3) . cells) testNb
    args <- getArgs
    case args of
        [input] -> stripOutputIO input "no_output.ipynb"
        [input, output] -> stripOutputIO input output
        _ -> stripOutputIO "test/data/hi.nbconvert.ipynb" "test/hi.cellout.ipynb"
        --_ -> putStrLn "please specify at least the input filename"




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
-- [x] current serialization doesn't match nbformat:
--      Unreadable Notebook: C:\bbg\jlabremix\tmp\hi.ipynb AttributeError('cell_type',)
-- [ ] when you have more than one execute_count output in a code cell, which one should be shown? all?
-- [x] Unreadable Notebook: C:\bbg\jlabremix\tmp\hi.ipynb UnboundLocalError("local variable 'newcell' referenced before assignment",)
-- [ ] probably remove the nbformat major/minor from the Notebook model and
--     have some mixin that does that at the end (most filters won't care about nbformat version)
--
-- from Anthony:
--     multiparameter type classes --
--     threadscope...
-- typeclasses - Show Eq -- Functor Traversable Applicative Monoid Monad
-- import Data.List hiding filter  -- from Anthony
--
-- [ ] add command-line parsing
-- [ ] pandoc integration?
--
-- 2018-10-12
-- [ ] upstream: execution_count redundancy at the cell and output level
--
-- 2018-10-17
-- [ ] separate input and output in different lists /  storage structures
-- what was my thought about each cell having its own ID - oh, right, storing
-- all of the text up front so you can edit it that way and then rejoin/resplit
-- it up (but allows for saving of output)
--
-- *Main Data.Aeson.Types> :set -XOverloadedStrings
-- > decode "{}"  :: Maybe Value
-- Just (Object (fromList []))
-- > decode "{\"a\":1}"  :: Maybe Value
-- Just (Object (fromList [("a",Number 1.0)]))

-- 2018-10-19
-- [x] strip output as the initial use case with IO

-- 2018-10-22
-- [ ] readNotebook >>> writeNotebook >>> ( reformat using nbconvert?) >>> diff with original
--     ^-- is to gauge what we're dropping on the floor right now, so we
--         eventually get to dropping nothing at all
-- [ ] add flags for --strip-output, for example, do do noop cellout
--
-- # roundtrip gaps
-- [ ] Execution count missing from In[ ] section
-- [ ] output stream text missing
-- [ ] encoding conversion for streams
--
-- 2018-11-02
-- [ ] --readonly / --read-only flags (no output - useful for )
-- [ ] --summary (number of cells, broken down by type, how many executed)
-- [ ] do we need some sort of executor abstraction down the line?
--
-- 2018-11-06
-- [ ] should we convert "source": [ "" ] to just "source": []?
-- [x] match up to reordered keys and whitespace for cleared output
--
-- 2018-11-26
-- [x] rework stream
-- [ ] replicate newline behavior for multiple source strings?
-- [x] preserve notebook metadata
--
-- 2018-11-27
-- [x] run `jq -S --indent 1` until I get the pretty printing figured out
--
-- 2018-12-17
-- [ ] better error message when failing to read notebook
