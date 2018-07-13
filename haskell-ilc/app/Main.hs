module Main where

import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.State.Strict
import Data.Semigroup ((<>))
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import Data.Monoid hiding ((<>))
import qualified Data.Map as Map
import Options.Applicative
import System.Console.Haskeline
import System.Exit
import System.Environment
import System.Console.Repline

import Eval
import Infer
import Parser
import Pretty
import Syntax
import Type

-------------------------------------------------------------------------------
-- Command line parser
-------------------------------------------------------------------------------

data CLOptions = CLOptions
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

optParser :: Parser CLOptions
optParser = CLOptions <$> inputFile <*> ast

opts :: ParserInfo CLOptions
opts = info (optParser <**> helper)
    ( fullDesc
    <> progDesc "Interactive Lambda Calculus (ILC) interpreter"
    <> header "ILC" )

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data IState = IState
    { tyenv :: Type.Env
    , tmenv :: TermEnv
    }

initState :: IState
initState = IState Type.empty emptyEnv -- TODO

type Repl a = HaskelineT (StateT IState IO) a
hoistErr :: Show a => Either e a -> Repl a
hoistErr (Right val) = return val
{-hoistErr (Left err) = do
    liftIO $ print err
    abort-}

-------------------------------------------------------------------------------
-- Execution
-------------------------------------------------------------------------------

evalDecl :: TermEnv -> Decl -> IO TermEnv
evalDecl env (DDecl x expr) = do
    v <- evalSub env expr
    return $ extendEnv env x v

--exec :: Bool ->     
    

process :: String -> IO ()
process src = do
  let cmds = parser src
  -- putStrLn $ show cmds
  case cmds of
    Left err -> print err
    Right cmds -> exec cmds >>= return . ppval >>= putStrLn

interactive :: IO ()
interactive = runInputT defaultSettings loop
  where
    loop = do
        minput <- getInputLine "\x03BB> "
        case minput of
            Nothing -> outputStrLn "Goodbye."
            Just input -> (liftIO $ process input ) >> loop

{-shell :: Repl a -> IO ()
shell pre = flip evalStateT initState
    $ evalRepl "Poly>" cmd options completer pre-}

main :: IO ()
main = do
    options <- execParser opts
    case (optSrcFile options) of
        Just file -> readFile file >>= process
        Nothing   -> interactive
        -- Nothing   -> shell
