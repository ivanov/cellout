import Cellout
import System.Exit as Exit
import Data.Aeson

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


