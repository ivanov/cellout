module CelloutSpec (spec) where

import Data.Aeson
import Test.Hspec

import Cellout

spec :: Spec 
spec = do
    describe "roundtrip to/fromJSON of " $ do
        it "in-memory test notebook" $ do
            case (fromJSON . toJSON) testNb of
                Success newNotebook ->
                    testNb `shouldBe` newNotebook
                Error err -> do
                    fail "failsed to read notebook"


