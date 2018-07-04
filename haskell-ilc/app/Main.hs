module Main where

import Data.Semigroup ((<>))
import Options.Applicative

--import Eval
import Parser
--import Pretty

data Options = Options
    { optSrcFile :: Maybe FilePath
    , optAst     :: Bool
    }

inputFile :: Parser (Maybe FilePath)
inputFile = optional $ strOption
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
    <> progDesc ""
    <> header "" )


main :: IO ()
main = do
    options <- execParser opts
    putStrLn "hi"
    {-prog    <- readFile $ optSrcFile options
    let prog' = rewrite prog
    case (optOutFile options) of
        Just outFile -> writeFile outFile prog'
        Nothing  -> putStrLn prog'-}

{-
import Control.Monad.Trans
import System.Console.Haskeline

process :: String -> IO ()
process line = do
  let res = parseExpr line
  case res of
    Left err -> print err
    Right ex -> case eval ex of
      Nothing -> putStrLn "Cannot evaluate"
      Just result -> putStrLn $ ppexpr result

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "\x03BB> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> (liftIO $ process input ) >> loop
-}
