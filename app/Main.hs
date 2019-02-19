{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Semigroup ((<>))
import Options.Applicative
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
--import Options.Applicative.Help

import qualified Cellout as C
--import qualified Database.Redis as R
import Database.Redis

data Opts = Options
    { inputFilename :: !FilePath
    , clearOutput :: !Bool
    , clearPrompt :: !Bool
    , readOnly :: !Bool
    , summary :: !Bool
    , outputFilename :: !String
    , outputType :: !String
    -- , nbformat :: !Int add nbformat option in the future
    , verbosity :: !Int
    } deriving Show

supported_outputs =
    [ "ipynb"
    , "notebook" -- alias for ipynb, from nbconvert
    , "py"
    , "script"
    , "markdown"
    ]

getCellsFilter :: Opts -> (C.Cell a -> C.Cell a)
getCellsFilter opts = case (clearOutput opts, clearPrompt opts) of
    (True, True) -> (C.clearOutput . C.clearPrompt)
    (True, False) -> C.clearOutput -- though this clears prompt also, at the moment
    (False, True) -> C.clearPrompt
    (False, False) -> id

main :: IO ()
main = do
    opts <- customExecParser p optsParser

    if (verbosity opts) > 2
    then putStrLn (show opts)
    else return ()

    if (verbosity opts) > 1
    then putStrLn ("Reading " ++ (inputFilename opts))
    else return ()

    -- there's probably some nicer way of handling the Left case here, but this
    -- is fine for now to make progress
    --Right nb <- readNb (inputFilename opts)  ; \x -> fail x
    -- TODO: Can't I leave this as readNb? the rest of the code doesn't care about the ambiguoutity of the a in (Notebook a) types
    res <- (C.readNb4 (inputFilename opts))
    case res of
        Left err -> do
            res2 <- (C.readNb3 (inputFilename opts))
            case res2 of
                Left err -> fail err
                Right nb -> withNotebook nb opts
        Right nb -> withNotebook nb opts

    where
        p :: ParserPrefs
        p = prefs (showHelpOnEmpty <> disambiguate)
        optsParser :: ParserInfo Opts
        optsParser =
            Options.Applicative.info
                (programOptions <* helper <* versionOption)
                (fullDesc <> progDesc "Convert to notebook formats" <>
                 header "cellout - buy into rich notebook conversion")
        versionOption :: Parser (a -> a)
        versionOption = infoOption "0.1.1" (long "version" <> help "show Version")
        programOptions :: Parser Opts
        programOptions =
            Options
            <$> strArgument
                (metavar "FILE" <> help "Input filename [required]") -- <> style bold)
            <*> switch
                (long "clear-output" <> help "Clear the output portion for all code cells")
            <*> switch
                (long "clear-prompt" <> long "no-prompt" <> help "Clear the In/Out prompt numbers all code cells")
            <*> switch
                (short 'r' <> long "read-only" <> long "readonly" <> help "do not write a result, useful for --summary")
            <*> switch
                (short 's' <> long "summary" <> help "Print details for the input notebook")
            <*> strOption
                (short 'o' <> long "output" <> metavar "RESULT" <> value "" <>
                help "Filename for the resulting notebook (defaults to writing in place)")
            <*> strOption
                (long "to" <> metavar "OUTPUT_TYPE" <> value "notebook" <> help ("format for the result, one of " ++ show supported_outputs))
            <*> (length <$> many (flag' () (long "verbose" <> short 'v' <> help "Print debugging messages. Multiple -v options increase the verbosity, up to a maximum of 5.")))


--withNotebook :: C.Notebook C.NbV4 -> Opts -> IO ()
withNotebook :: A.ToJSON (C.Notebook a) => C.Notebook a -> Opts -> IO ()
withNotebook nb opts = do
    info <- return (C.collectInfo nb)

    if (verbosity opts) > 2 || (summary opts)
    then putStrLn ("Input notebook contains " ++ show info)
    else return ()


    -- TODO: we don't yet actually write the file
    if (not $ readOnly opts)
    then let output = if (outputFilename opts) == "" then (inputFilename opts) else (outputFilename opts)
             f = getCellsFilter opts
             outNb = C.cellsFilter (fmap f) nb
             outInfo = C.collectInfo outNb
        in do
            C.writeNb output outNb

            if (verbosity opts) > 0
            then putStrLn ("Wrote " ++ output)
            else return ()

            if (verbosity opts) > 2 || (summary opts)
            then putStrLn ("Output notebook contains " ++ show outInfo)
            else return ()

    else writeStatsToDb info opts

writeStatsToDb :: C.NotebookInfo -> Opts ->  IO ()
writeStatsToDb info opts = do
    conn <- checkedConnect defaultConnectInfo
    runRedis conn $ do
        new <- sadd "notebooks" [((encodeUtf8 . pack . inputFilename) opts)]
        case new of
            Right num -> 
                if num == 1 then do
                    incrby "number_notebooks" 1 
                    incrby "markdown" (toInteger (C.nMarkdownCells info))
                    incrby "code" (toInteger (C.nCodeCells info))
                    incrby "raw" (toInteger (C.nRawCells info))
                    return ()
                else do
                    liftIO $ putStrLn("Already processed " ++ inputFilename opts ++ "  before")
            Left reply -> fail (show reply)
        -- hincrby "mimetypes"  mimetype 1
    pure ()


