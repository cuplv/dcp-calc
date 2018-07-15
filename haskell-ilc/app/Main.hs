{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.State.Strict
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import Data.List (isPrefixOf, foldl')
import Data.Monoid
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
hoistErr :: Show e => Either e a -> Repl a
hoistErr (Right val) = return val
hoistErr (Left err) = do
    liftIO $ print err
    abort

-------------------------------------------------------------------------------
-- Execution
-------------------------------------------------------------------------------

evalDecl :: TermEnv -> Decl -> IO TermEnv
evalDecl env (x, expr) = do
    v <- evalSub env expr
    let env' = extendEnv env x v
    return $ env'
    
exec :: Bool -> String -> Repl ()
exec update source = do
    st <- get
    mod <- hoistErr $ parser source
    
    tyenv' <- hoistErr $ inferTop (tyenv st) mod

    tmenv' <- liftIO $ foldM (evalDecl) (tmenv st) mod
    
    let st' = st { tmenv = tmenv'
                 , tyenv = tyenv' <> (tyenv st)
                 }

    when update (put st')
    
    case Prelude.lookup "it" mod of
      Nothing -> return ()
      Just ex -> do
        val <- liftIO $ evalSub (tmenv st') ex
        showOutput (show val) st'

showOutput :: String -> IState -> Repl ()
showOutput arg st = do
  case Type.lookup "it" (tyenv st)  of
    Just val -> liftIO $ putStrLn $ ppsignature (arg, val)
    Nothing -> return ()
    
cmd :: String -> Repl ()
cmd source = Main.exec True source

process :: String -> IO ()
process src = do
  let cmds = parser src
  case cmds of
    Left err -> print err
    Right cmds -> Eval.exec cmds >>= return . ppval >>= putStrLn

-------------------------------------------------------------------------------
-- Commands
-------------------------------------------------------------------------------

-- :browse command
browse :: [String] -> Repl ()
browse _ = do
  st <- get
  liftIO $ mapM_ putStrLn $ ppenv (tyenv st)

-- :load command
load :: [String] -> Repl ()
load args = do
  contents <- liftIO $ readFile (unwords args)
  Main.exec True contents

-- :type command
typeof :: [String] -> Repl ()
typeof args = do
  st <- get
  let arg = unwords args
  case Type.lookup arg (tyenv st) of
    Just val -> liftIO $ putStrLn $ ppsignature (arg, val)
    Nothing -> Main.exec False arg

-- :quit command
quit :: a -> Repl ()
quit _ = liftIO $ exitSuccess

-------------------------------------------------------------------------------
-- Interactive Shell
-------------------------------------------------------------------------------

-- Prefix tab completer
defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = [
    (":load"  , fileCompleter)
  -- , (":type"  , values)
  ]

-- Default tab completer
comp :: (Monad m, MonadState IState m) => WordCompleter m
comp n = do
  let cmds = [":load", ":type", ":browse", ":quit"]
  TypeEnv ctx <- gets tyenv
  let defs = Map.keys ctx
  return $ filter (isPrefixOf n) (cmds ++ defs)

options :: [(String, [String] -> Repl ())]
options = [
    ("load"   , load)
  , ("browse" , browse)
  , ("quit"   , quit)
  , ("type"   , Main.typeof)
  ]

completer :: CompleterStyle (StateT IState IO)
completer = Prefix (wordCompleter comp) defaultMatcher

shell :: Repl a -> IO ()
shell pre = flip evalStateT initState 
    $ evalRepl "\x03BB: " cmd options Main.completer pre

main :: IO ()
main = do
    options <- execParser opts
    case (optSrcFile options) of
        Just file -> readFile file >>= process
        -- Nothing   -> interactive
        Nothing   -> shell (return ())
