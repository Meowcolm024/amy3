module Main where

import           Options.Applicative
import           Pipe
import           Utils                          ( evalError )

data Mode = Interpret | Js deriving Show

data Opts = Opts
    { inputFiles :: [String]
    , mode       :: Mode
    , optmize    :: Bool
    , output     :: Maybe String
    }

main :: IO ()
main = entry =<< execParser opts
  where
    opts = info
        (cli <**> helper)
        (fullDesc <> progDesc des <> header
            "amy3 - The amy3 language interpreter/compiler"
        )
    des =
        "amy3 is a subset of the Scala Programming Language. "
            ++ "It can be directly interpreted or compiled to JavaScript"


entry :: Opts -> IO ()
entry (Opts [] _ _ _) = evalError "Not input file!"
entry (Opts fs m _ _) = case m of
    Interpret -> interpretMode fs
    Js        -> error "not implemented"

cli :: Parser Opts
cli =
    Opts
        <$> many (argument str (metavar "TARGET..."))
        <*> flag Js
                 Interpret
                 (long "interpret" <> short 'i' <> help "Interpret program")
        <*> switch (long "optimize" <> short 'O' <> help "Turn on optimization")
        <*> optional
                (  strOption
                $  long "output"
                <> short 'o'
                <> metavar "FILE"
                <> value "out.js"
                <> help "Write output to FILE"
                )

