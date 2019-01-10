module Cellout.JSON where

import Data.Foldable (asum)
import Control.Applicative -- ((<|>))
import Data.Text.Encoding (decodeUtf8)
import Cellout.Types
import Data.Aeson
import Data.Aeson.Types
-- import qualified Data.Aeson.Types.Internal (Parser)
import Data.Aeson.Encode.Pretty
import Data.List (isSuffixOf)
import Data.Vector as V ((!))
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Lazy as HML
-- TODO: get rid of the HML usage here - there must be a better API for this
import qualified Data.Text as T


instance ToJSON StreamName where
    toJSON = genericToJSON defaultOptions{
        constructorTagModifier = toLower
    }

instance FromJSON StreamName where
    parseJSON = genericParseJSON defaultOptions{
        constructorTagModifier = toLower
    }


metaCorrector  :: String -> String
metaCorrector x =
    if isSuffixOf "metadata" x
    then "metadata"
    else x

dataKeywordFix :: String -> String
dataKeywordFix "data_" = "data"
dataKeywordFix x = x

toLower = (T.unpack . T.toLower . T.pack)

opts = defaultOptions{ fieldLabelModifier = metaCorrector }

cellsFromWorksheet :: Value -> Parser [Cell]
cellsFromWorksheet = withArray "worksheets array" $ \a -> do
        --obj <- parseIndexedJSON a 0
        --obj <- pure a !! 0
        let val = a ! 0
        --obj .: T.pack "cells"
        withObject "worksheet object" (\obj -> (obj .: T.pack "cells")) val

--nb3_Parser :: Value -> Parser Notebook
nb3_Parser = withObject "Notebook object" $ \v ->
    --Notebook <$> ( cellsFromWorksheet <$> (v .: T.pack "worksheets"))
    Notebook <$> (\() -> do
                    ws <- v .: T.pack "worksheets"
                    cellsFromWorksheet ws) ()
             <*>  v .:? T.pack "metadata" .!= mempty
             <*>  v .:? T.pack "nbformat" .!= 3
             <*>  v .:? T.pack "nbformat_minor" .!= 0

-- TODO: combine multiple notebook parsers, since version 3 had cells under "worksheet"
-- or do I want to make Notebook4 and Notebook3 separte instances, and have Notebook be a union of those two?
instance FromJSON Notebook where
    parseJSON x = modifyFailure ("Sorry, I can't parse this notebook: " ++) (genericParseJSON opts x) <|> (nb3_Parser x)
    --parseJSON x = (genericParseJSON opts x) <|> (nb3_Parser x)
    --parseJSON = asum [(genericParseJSON opts), nb3_Parser]
instance ToJSON Notebook where
    toEncoding = genericToEncoding opts
    toJSON = genericToJSON opts

-- try the following
--

cell_type = T.pack "cell_type"
outputs = T.pack "outputs"



--instance FromJSON Cell --where
instance FromJSON Cell where
    --  parseJSON = genericParseJSON defaultOptions  {
    --    }
    parseJSON =  withObject "Cell object" $ \v -> do
        cell_type <- v .: T.pack "cell_type"
        case cell_type of
            "markdown" -> MarkdownCell <$> (parseJSON (Object v) )
            "code" -> CodeCell <$> (parseJSON (Object v) ) <*>  (v .: T.pack "outputs") <*>  parseJSON (Object v)
            "raw" -> RawCell <$> (parseJSON (Object v) )
            _ ->  fail ("unrecognized cell type" ++ cell_type)

instance ToJSON Cell where
    -- toJSON (MarkdownCell c) = object $ [ cell_type .= "markdown" ]
    -- toJSON (CodeCell c o i) = object $ [ cell_type .= "code" ]
    toJSON (MarkdownCell c) = Object $ HML.insert cell_type (toJSON "markdown") (unobject $ toJSON c)
    toJSON (RawCell c) = Object $ HML.insert cell_type (toJSON "raw") (unobject $ toJSON c)
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

nb3_parseCommon :: Value -> Parser CommonCellContent
nb3_parseCommon  = withObject "common cell content" $ \o -> do
    CommonCellContent <$> o .: T.pack "input" <*> o .: T.pack "metadata"

instance FromJSON CommonCellContent where
    parseJSON x = genericParseJSON opts x <|> nb3_parseCommon x
instance ToJSON CommonCellContent where
    -- toEncoding = genericToEncoding opts
    toEncoding = genericToEncoding opts
    toJSON = genericToJSON opts

outputTagMod :: String -> String
outputTagMod "ExecuteResult" = "execute_result"
outputTagMod "DisplayData" = "display_data"
outputTagMod "ErrorData" = "error"
outputTagMod x = toLower x

nb3_outputTagMod :: String -> String
nb3_outputTagMod "ExecuteResult" = "pyout"
nb3_outputTagMod "ErrorData" = "pyerr"
nb3_outputTagMod x =  outputTagMod x


instance FromJSON Output where
    parseJSON x = genericParseJSON defaultOptions{
        sumEncoding = TaggedObject "output_type" "",
        -- I want taggedflattened object, but instead rewrote  Output to have records
        unwrapUnaryRecords = True,
        fieldLabelModifier = metaCorrector . dataKeywordFix . nb3_streamRenamer,
        constructorTagModifier = outputTagMod -- . nb3_outputTagMod
        } x <|>
        -- nbformat3 parser
        genericParseJSON defaultOptions{
        sumEncoding = TaggedObject "output_type" "",
        unwrapUnaryRecords = True,
        fieldLabelModifier = metaCorrector . dataKeywordFix . nb3_streamRenamer,
        constructorTagModifier = nb3_outputTagMod
        } x
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

nb3_streamRenamer :: String -> String
nb3_streamRenamer "name" = "stream"
nb3_streamRenamer x = x

instance ToJSON Output where
    toJSON = genericToJSON defaultOptions{
        -- sumEncoding = UntaggedValue,
        sumEncoding = TaggedObject "output_type" "",
        -- I want taggedflattened object, but instead rewrote  Output to have records
        unwrapUnaryRecords = True,
        fieldLabelModifier = metaCorrector . dataKeywordFix,
        constructorTagModifier = outputTagMod . nb3_outputTagMod
        }


nb3_executionCount :: Object -> Parser (Maybe Int)
nb3_executionCount v =  v .:? T.pack "prompt_number" .!= Nothing

instance FromJSON ExecutionCount where
    -- parseJSON = genericParseJSON defaultOptions{
    --     sumEncoding = ObjectWithSingleField,
    --     tagSingleConstructors = True,
    --     constructorTagModifier = \x -> if x == "execution_count" then "ExecutionCount" else x
    -- }
    -- -- ARGH, so frustrating that I can't figure out how to derive this and keep things generic
    parseJSON (Object v) = ExecutionCount <$>  (v .: T.pack "execution_count")  <|>  ExecutionCount <$> (nb3_executionCount v)
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


encode' :: ToJSON a => a -> LB.ByteString
encode' = encodePretty' defConfig{confIndent=Spaces 1, confCompare=compare}

writeNb :: FilePath -> Notebook -> IO ()
writeNb file nb = do
    if (version /= 4)
    then
        putStrLn $ "WARNING: I don't necessarily  know how to write notebook version" ++ (show version)
    else
        pure ()
    LB.writeFile file (encode' nb)
    where version = nbformat nb

readNb :: FilePath -> IO (Either String Notebook)
readNb f = do
    input <- LB.readFile f
    return (eitherDecode input)

readNb' :: FilePath -> IO (Maybe Notebook)
readNb' f = do
    input <- LB.readFile f
    return (decode input)


nbAsJSONString :: Notebook -> String
nbAsJSONString = T.unpack . decodeUtf8 . LB.toStrict . encode
