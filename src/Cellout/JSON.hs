-- {-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
module Cellout.JSON where

import Data.Foldable (asum)
import Control.Applicative -- ((<|>))
import Data.Text.Encoding (decodeUtf8)
-- import Cellout.Types
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

import Data.Ipynb


-- TODO: try this without pretty to see Data.Ipynb default spacing
encode' :: ToJSON a => a -> LB.ByteString
encode' = encodePretty' defConfig{confIndent=Spaces 1, confCompare=compare}


-- TODO - is there a different way of doing this? if we specify the notebook
-- version, there's no need for the ToJSON type onstraint
-- writeNb :: FilePath -> Notebook NbV4 -> IO ()
-- TODO: also - instead of returning IO Unit, we should return wether
-- or not we succeeded in writing the file
writeNb :: ToJSON (Notebook a) => FilePath -> Notebook a -> IO ()
writeNb file nb = do
    LB.writeFile file (encode' nb)

writeNb3 :: FilePath -> Notebook NbV3 -> IO ()
writeNb3 file nb = do
    LB.writeFile file (encode' nb)

writeNb4 :: FilePath -> Notebook NbV4 -> IO ()
writeNb4 file nb = do
    LB.writeFile file (encode' nb)

    -- where version = fst (notebookFormat nb)
readNb :: FromJSON (Notebook a) => FilePath -> IO (Either String (Notebook a))
readNb f = do
    input <- LB.readFile f
    return (eitherDecode input)
    
readNb3 :: FilePath -> IO (Either String (Notebook NbV3))
readNb3 f = do
    input <- LB.readFile f
    return (eitherDecode input)
    
readNb4 :: FilePath -> IO (Either String (Notebook NbV4))
readNb4 f = do
    input <- LB.readFile f
    return (eitherDecode input)

readNb' :: FilePath -> IO (Maybe (Notebook NbV4))
readNb' f = do
    input <- LB.readFile f
    return (decode input)


nbAsJSONString :: ToJSON (Notebook a) => Notebook a -> String
nbAsJSONString = T.unpack . decodeUtf8 . LB.toStrict . encode



