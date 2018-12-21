module CelloutSpec (spec) where

import Control.Monad.IO.Class
import Test.Hspec

import Cellout
import Data.Aeson


roundtrip :: Notebook -> Result Notebook
roundtrip = fromJSON . toJSON

spec :: Spec
spec = do
    describe "roundtrip to/fromJSON of " $ do
        it "in-memory test of trivial notebook" $ do
            case roundtrip trivialNb of
                Success newNotebook ->
                    trivialNb `shouldBe` newNotebook
                Error err -> do
                    "Could not read notebok" `shouldBe` err

        it "in-memory test of notebook" $ do
            case roundtrip testNb of
                Success newNotebook ->
                    testNb `shouldBe` newNotebook
                Error err -> do
                    "Could not read notebok" `shouldBe` err

    spec_fromDisk


spec_fromDisk :: Spec
spec_fromDisk = do
    describe "reading sample notebooks" $ do
        it "can read notebook from disk" $ do
            res <- liftIO $ readNb' "test/data/hi.ipynb"
            case res of
                Just nb -> (length . cells) nb `shouldBe` 10
                Nothing ->  "Could not read notebok" `shouldBe` ""

        it "can read notebooks with all three cell types" $ do
            res <- liftIO $ readNb' "test/data/empties.ipynb"
            case res of
                Just nb -> (length . cells) nb `shouldBe` 3
                Nothing ->  "Could not read notebok" `shouldBe` ""

        it "can read notebooks with all three cell types" $ do
            res <- liftIO $ readNb "test/data/empties.ipynb"
            case res of
                Right nb -> (length . cells) nb `shouldBe` 3
                Left err ->  "Could not read notebok" `shouldBe` err
