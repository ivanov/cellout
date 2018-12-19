module CelloutSpec (spec) where

import Control.Monad.IO.Class
import Test.Hspec

import Cellout
import Data.Aeson

spec :: Spec
spec = do
    describe "roundtrip to/fromJSON of " $ do
        it "in-memory test of trivial notebook" $ do
            case (fromJSON . toJSON) trivialNb of
                Success newNotebook ->
                    trivialNb `shouldBe` newNotebook
                Error err -> do
                    fail "failed to read notebook"

        it "in-memory test of notebook" $ do
            case (fromJSON . toJSON) testNb of
                Success newNotebook ->
                    testNb `shouldBe` newNotebook
                Error err -> do
                    fail "failed to read notebook"
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
