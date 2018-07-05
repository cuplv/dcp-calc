module Main where

import Control.Monad.Trans
import Data.Semigroup ((<>))
import Options.Applicative
import System.Console.Haskeline

import Eval
import Parser
import Pretty

data Options = Options
    { optSrcFile :: Maybe FilePath
    , optAst     :: Bool
    }

inputFile :: Parser (Maybe FilePath)
inputFile = optional $ argument str
    (  metavar "FILENAME"
    <> help "Source file" )

ast :: Parser Bool
ast = switch
  (  long "ast"
  <> help "Print abstract syntax tree" )

optParser :: Parser Options
optParser = Options <$> inputFile <*> ast

opts :: ParserInfo Options
opts = info (optParser <**> helper)
    ( fullDesc
    <> progDesc "Interactive Lambda Calculus (ILC) interpreter"
    <> header "ILC" )

-- | TODO: Refactor this.
process :: String -> IO ()
process src = do
  let ast = parseExpr src
  putStrLn $ show ast
{-  case ast of
    Left err -> print err
    Right p  -> do let v = eval [] p
                   putStrLn $ ppval v-}

interactive :: IO ()
interactive = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "\x03BB> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> (liftIO $ process input ) >> loop

main :: IO ()
main = do
    options <- execParser opts
    case (optSrcFile options) of
        Just file -> readFile file >>= \ src ->
                     process src
        Nothing   -> interactive
