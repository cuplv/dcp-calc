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
                                 assertEqual "" (parser src) (ast)
                           
parserExamples =
    [ ( "lambda"
      , "lam x . x + x"
      , Right [ CExpr (ELam (EVar "x")
                            (EPlus (EVar "x")
                                   (EVar "x")))
              ]
      )
    , ( "allocate channel then write"
      , "nu c . wr 1 -> c"
      , Right [ CExpr (ENu (EVar "c")
                           (EWr (EInt 1)
                                (EVar "c")))
              ]
      )
    , ( "let binding"
      , "let x = 100 in x * 1"
      , Right [ CExpr (ELet (PVar "x")
                            (EInt 100)
                            (ETimes (EVar "x")
                                     (EInt 1)))
              ]
      )
    , ( "let binding w/ tuple matching"
      , "let (x, y) = (1, 2) in x + y"
      , Right [ CExpr (ELet (PTuple [PVar "x", PVar "y"])
                            (ETuple [EInt 1, EInt 2])
                            (EPlus (EVar "x")
                                   (EVar "y")))
              ]
      )
    , ( "let binding w/ unit and function application"
      , "let () = \"whatever\" in double 2"
      , Right [ CExpr (ELet (PUnit)
                            (EString "whatever")
                            (EApp (EVar "double")
                                  (EInt 2)))
              ]
      )
    , ( "sequencing let bindings"
      , "let x = 1 in x; let y = 1 in y"
      , Right [ CExpr (ELet (PVar "x")
                            (EInt 1)
                            (ESeq (EVar "x")
                                  (ELet (PVar "y")
                                        (EInt 1)
                                        (EVar "y"))))
              ]
      )
    , ( "nested let bindings"
      , "let x = 1 in let y = 2 in x + y"
      , Right [ CExpr (ELet (PVar "x")
                            (EInt 1)
                            (ELet (PVar "y")
                                  (EInt 2)
                                  (EPlus (EVar "x")
                                         (EVar "y"))))
              ]
      )
    , ( "let commands"
      , "let x = 1 let y = 2 let z = x + y"
      , Right [ CDef (PVar "x")
                     (EInt 1)
              , CDef (PVar "y")
                     (EInt 2)
              , CDef (PVar "z")
                     (EPlus (EVar "x")
                            (EVar "y"))
              ]
      )
    , ( "let command, let binding, expr command"
      , "let _ = let x = 1 in 2 * x let y = 1;; \"foo\""
      , Right [ CDef (PWildcard)
                     (ELet (PVar "x")
                           (EInt 1)
                           (ETimes (EInt 2)
                                   (EVar "x")))
              , CDef (PVar "y")
                     (EInt 1)
              , CExpr (EString "foo")
              ]
      )
    , ( "expr commands and sequencing"
      , "1 ; 2 ;; 3 ; 4"
      , Right [ CExpr (ESeq (EInt 1)
                            (EInt 2))
              , CExpr (ESeq (EInt 3)
                            (EInt 4))
              ]
      )
    ]
