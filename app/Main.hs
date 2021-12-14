module Main where

import           Control.Monad                  ( when )
import           Data.Maybe                     ( isJust )
import qualified Data.Text.IO                  as TIO
import           Mode
import           Options.Applicative
import           System.Exit                    ( exitFailure )
import           System.IO
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
    opts = info (cli <**> helper) (fullDesc <> progDesc des <> header hd)
    des =
        "amy3 is a subset of the Scala Programming Language. "
            ++ "It can be directly interpreted or compiled to JavaScript"
    hd = "amy3 - The amy3 language interpreter/compiler"

entry :: Opts -> IO ()
entry (Opts [] _ _ _) =
    hPutStrLn stderr "amy3: error: no input files" *> exitFailure
entry (Opts fs m op ot) = case m of
    Interpret -> do
        when op
            $ putStrLn
                  "[Warning] Optimization is not supported in interpret mode, ignoring..."
        when (isJust ot)
            $ putStrLn
                  "[Warning] No output will be generated when interpreted, ignoring..."
        interpretMode fs
    Js -> do
        prog <- codeGenMode op fs
        case ot of
            Just f  -> TIO.writeFile f prog
            Nothing -> do
                putStrLn "[Warning] No output file indicated, write to 'out.js'"
                TIO.writeFile "out.js" prog

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
                <> help "Write output to FILE"
                )
