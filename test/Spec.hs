import Cellout
import System.Exit as Exit
import Data.Aeson
import Test.QuickCheck (generate, elements, choose)


-- generate some random:
-- - markdown
-- - code
-- - output
-- - metadata
--
-- insert unicode and other funky characters in it
-- (I want to feel all warm and fuzzy)

mods = generate $ elements ["code", "raw", "markdown"]

num_cells = generate $ choose (1 :: Int,20)



get_cells :: Int -> IO [ Cell ]
get_cells 0 = []
get_cells i = return [ MarkdownCell mempty ] ++ get_cells (i-1)

main :: IO ()
main = do
    putStrLn "Test suite not yet implemented"
    -- want some sort of assert here... for testNb == (decode . encode) testNb
    case (fromJSON . toJSON) testNb of
        Success newNotebook ->
            if testNb == newNotebook
                then Exit.exitSuccess
                else Exit.exitFailure
        Error err -> do
            putStrLn err
            putStrLn (show (toJSON testNb))
            Exit.exitFailure


