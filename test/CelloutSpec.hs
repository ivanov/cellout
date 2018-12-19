module CelloutSpec (spec) where

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
