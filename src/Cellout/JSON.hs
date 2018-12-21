module Cellout.JSON where

import Cellout.Types
import Data.Aeson
import Data.List (isSuffixOf)
import qualified Data.HashMap.Lazy as HML
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

instance FromJSON Notebook where
    parseJSON = genericParseJSON  defaultOptions{ fieldLabelModifier = metaCorrector }
instance ToJSON Notebook where
    toEncoding = genericToEncoding defaultOptions{ fieldLabelModifier = metaCorrector }
    toJSON = genericToJSON defaultOptions{ fieldLabelModifier = metaCorrector }


cell_type = T.pack "cell_type"
outputs = T.pack "outputs"



--instance FromJSON Cell --where
instance FromJSON Cell where
    --  parseJSON = genericParseJSON defaultOptions  {
    --    }
    parseJSON (Object v) =  do
        cell_type <- v .: T.pack "cell_type"
        case cell_type of
            "markdown" -> MarkdownCell <$> (parseJSON (Object v) )
            "code" -> CodeCell <$> (parseJSON (Object v) ) <*>  (v .: T.pack "outputs") <*>  parseJSON (Object v)
            "raw" -> RawCell <$> (parseJSON (Object v) )
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


