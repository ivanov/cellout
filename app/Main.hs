module Main where

import Data.Semigroup ((<>))
import Options.Applicative
--import Options.Applicative.Help

import qualified Cellout as C

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

getCellsFilter :: Opts -> (C.Cell -> C.Cell)
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
    Just nb <- C.readNb' (inputFilename opts)

    info <- return (C.collectInfo nb)

    if (verbosity opts) > 2 || (summary opts)
    then putStrLn ("Input notebook contains " ++ info)
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
            then putStrLn ("Output notebook contains " ++ outInfo)
            else return ()

    else return ()

    where
        p :: ParserPrefs
        p = prefs showHelpOnEmpty
        optsParser :: ParserInfo Opts
        optsParser =
            info
                (programOptions <* helper <* versionOption)
                (fullDesc <> progDesc "Convert to notebook formats" <>
                 header "cellout - buy into rich notebook conversion")
        versionOption :: Parser (a -> a)
        versionOption = infoOption "0.1" (long "version" <> help "show Version")
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
                (long "to" <> metavar "OUTPUT_TYPE" <> value "notebook" <> help ("format for the result, on of " ++ show supported_outputs))
            <*> (length <$> many (flag' () (long "verbose" <> short 'v' <> help "Print debugging messages. Multiple -v options increase the verbosity, up to a maximum of 5.")))


--- NEAT
--
-- Using bash auto-completion
-- Every program using optparse-applicative gets hidden arguments to support bash auto-completion. For your own personal use, you run this or add it this to your .bashrc:
--
-- eval "$(myprog --bash-completion-script myprog)"
-- To install the bash completion system-wide, put the output of --bash-completion-script where your system looks for it. For example, on Ubuntu:
--
-- myprog --bash-completion-script myprog >/etc/bash_completion.d/myprog)
--
-- TODO
-- [x] - reorder helper and versionOption to put them at the end
-- [ ] - accept multiple filenames?
-- [ ] - is it possible to show help on error only for the empty case?
