module CelloutSpec (spec) where

import Control.Monad.IO.Class
import Test.Hspec

import Cellout
import Data.Aeson


roundtrip :: Notebook -> Result Notebook
roundtrip = fromJSON . toJSON

spec :: Spec
spec = do
    spec_JSON
    spec_fromDisk
    spec_notebookOps

spec_JSON :: Spec
spec_JSON = do
    describe "roundtrip to/fromJSON of " $ do
        it "in-memory test of trivial notebook" $ do
            case roundtrip trivialNb of
                Success newNotebook ->
                    trivialNb `shouldBe` newNotebook
                Error err -> do
                    "Could not read notebook" `shouldBe` err

        it "in-memory test of notebook" $ do
            case roundtrip testNb of
                Success newNotebook ->
                    testNb `shouldBe` newNotebook
                Error err -> do
                    "Could not read notebook" `shouldBe` err

spec_fromDisk :: Spec
spec_fromDisk = do
    describe "reading sample notebooks" $ do
        it "can read notebook from disk" $ do
            res <- liftIO $ readNb' "test/data/hi.ipynb"
            case res of
                Just nb -> (length . cells) nb `shouldBe` 10
                Nothing ->  "Could not read notebook" `shouldBe` ""

        it "can read notebooks with all three cell types" $ do
            res <- liftIO $ readNb "test/data/empties.ipynb"
            case res of
                Right nb -> (length . cells) nb `shouldBe` 3
                Left err ->  "Could not read notebook" `shouldBe` err

        it "can read notebooks with rich output" $ do
            res <- liftIO $ readNb "test/data/Rich Output.ipynb"
            case res of
                Right nb -> (length . cells) nb `shouldBe` 77
                Left err ->  "Could not read notebook" `shouldBe` err

        it "can read notebooks with error output" $ do
            res <- liftIO $ readNb "test/data/error.ipynb"
            case res of
                Right nb -> (length . cells) nb `shouldBe` 1
                Left err ->  "Could not read notebook" `shouldBe` err

spec_notebookOps :: Spec
spec_notebookOps = do
    describe "notebook operations" $ do
        it "can create notebooks via `mempty` or `mempty :: Notebook`" $ do
            (length . cells) (mempty :: Notebook) `shouldBe` 0
            (length . cells) mempty `shouldBe` 0
        it "<> of notebook with itself should double the number of cells" $ do
            let numCells = (length . cells) testNb
            (length . cells) (testNb <> testNb) `shouldBe` 2 * numCells
