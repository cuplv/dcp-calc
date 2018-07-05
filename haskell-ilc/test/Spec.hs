module Main where

import Text.Printf
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC

import Syntax
import Parser

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [parserTests]

parserTests :: TestTree
parserTests =
    testGroup "Parser tests" $ makeParserTests

makeParserTests = map f parserExamples
  where f test = case test of
            (str, src, ast) -> testCase (printf "parse %s" str) $
                                 assertEqual "" (parseExpr src) (ast)
                           
parserExamples =
    [ ( "lambda"
      , "lam x . x + x"
      , Right (ELam (EVar "x") (EPlus (EVar "x") (EVar "x")))
      )
    , ( "allocate channel then write"
      , "nu c . wr 1 -> c"
      , Right (ENu (EVar "c") (EWr (EInt 1) (EVar "c")))
      )
    , ( "let binding"
      , "let x = 100 in x + 1"
      , Right (ELet (PVar "x") (EInt 100) (EPlus (EVar "x") (EInt 1)))
      )
    , ( "let binding w/ tuple matching"
      , "let (x, y) = (1, 2) in x + y"
      , Right (ELet (PTuple [PVar "x", PVar "y"])
                    (ETuple [EInt 1, EInt 2])
                    (EPlus (EVar "x") (EVar "y")))
      )
    , ( "let binding w/ unit and function application"
      , "let () = \"whatever\" in double 2"
      , Right (ELet (PUnit)
                    (EString "whatever")
                    (EApp (EVar "double") (EInt 2)))
      )
    ]
