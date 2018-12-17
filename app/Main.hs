module Main where

import Data.Semigroup ((<>))
import Options.Applicative
--import Options.Applicative.Help

data Opts = Opts
    { input :: !FilePath
    , clearOutput :: !Bool
    , outputFilename :: !String
    , verbose :: !Int
    }

main :: IO ()
main = do
    --opts <- execParser optsParser
    opts <- customExecParser p optsParser
    if (verbose opts) > 0
    then putStrLn
        (concat
        ["Hey there, "
        , input opts
        , ", the flag is "
        , outputFilename  opts
        , ", the flag is "
        , show (clearOutput opts)
        , "verbosity level is"
        , show (verbose opts)
        ])
    else putStrLn ""

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
            Opts
            <$> strArgument
                (metavar "FILE" <> help "Input filename [required]") -- <> style bold)
            <*> switch
                (long "clear-output" <> help "Clear the output portion for all code cells")
            <*> strOption
                (short 'o' <> long "output" <> metavar "RESULT" <> value "" <>
                help "Filename for the resulting notebook (defaults to writing in place)")
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
