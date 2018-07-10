module Main where

import Text.Printf
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC

import Syntax
import Parser
import Eval

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [parserTests, pmTests]

parserTests :: TestTree
parserTests =
    testGroup "Parser tests" $ makeParserTests

makeParserTests = map f parserExamples
  where f (str, src, ast) = testCase (printf "parse %s" str) $
                            assertEqual "" (parser src) (ast)
                           
parserExamples =
    [ ( "lambda"
      , "lam x . x + x"
      , Right [ CExpr (ELam (PVar "x")
                            (EPlus (EVar "x")
                                   (EVar "x")))
              ]
      )
    , ( "allocate channel then write"
      , "nu c . wr 1 -> c"
      , Right [ CExpr (ENu "c"
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
      , Right [ CDef "x"
                     (EInt 1)
              , CDef "y"
                     (EInt 2)
              , CDef "z"
                     (EPlus (EVar "x")
                            (EVar "y"))
              ]
      )
    , ( "let command, let binding, expr command"
      , "let z = let x = 1 in 2 * x let y = 1;; \"foo\""
      , Right [ CDef "z"
                     (ELet (PVar "x")
                           (EInt 1)
                           (ETimes (EInt 2)
                                   (EVar "x")))
              , CDef "y"
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
    , ( "pattern matching"
      , "match b with | 0 => \"zero\" | 1 => \"one\""
      , Right [ CExpr (EMatch (EVar "b")
                              ([ (PInt 0, EBool True, EString "zero")
                               , (PInt 1, EBool True, EString "one")
                               ]))
              ]
      )
    , ( "let binding w/ assign"
      , "let x = 1 ; let y := 1 in x + y"
      , Right [ CExpr (ELet (PVar "x")
                            (ESeq (EInt 1)
                                  (EAssign (PVar "y")
                                           (EInt 1)))
                            (EPlus (EVar "x")
                                   (EVar "y")))
              ]
      )
    , ( "ref and deref"
      , "let a = ref 1 ;; let b := @ a"
      , Right [ CDef "a"
                     (ERef (EInt 1))
              , CExpr (EAssign (PVar "b")
                               (EDeref (EVar "a")))
              ]
      )
    , ( "let binding w/ sequencing and assign"
      , "let a = 1 ; let b := 1 in b"
      , Right [ CExpr (ELet (PVar "a")
                            (ESeq (EInt 1)
                                  (EAssign (PVar "b")
                                           (EInt 1)))
                            (EVar "b"))
              ]
      )
    , ( "cons pattern matching"
      , "match a with | [] => 0 | x:xs => 1"
      , Right [ CExpr (EMatch (EVar "a")
                              ([ (PList [], EBool True, EInt 0)
                               , (PCons (PVar "x")
                                        (PVar "xs"), EBool True, EInt 1)
                               ]))
              ]
      )
    , ( "pattern matching with guards"
      , "match b with | 0 when 0 < 1 => 0 | 1 when true => 1"
      , Right [ CExpr (EMatch (EVar "b")
                              ([ (PInt 0, ELt (EInt 0)
                                              (EInt 1), EInt 0)
                               , (PInt 1, EBool True, EInt 1)
                               ]))
              ]
      )
    , ( "plus function w/ type signature"
      , "plus :: Int -> Int -> Int let plus = lam x . lam y . x + y"
      ,  Right [ CTySig "plus" (TArrow TInt (TArrow TInt TInt))
               , CDef "plus"
                      (ELam (PVar "x")
                            (ELam (PVar "y")
                                  (EPlus (EVar "x") (EVar "y"))))
              ]
      )
    , ( "GetBit function signature w/ Wr mode"
      , "GetBit :: Int -> Wr Int"
      , Right [ CTySig "GetBit" (TArrow TInt (TWr TInt)) ]
      )
    , ( "GetBit"
      , "let GetBit = lam x . nu c . |> (rd c) ; |> (wr 0 -> c) ; |> (wr 1 -> c) GetBit 1"
      , Right [ CDef "GetBit"
                     (ELam (PVar "x")
                           (ENu "c"
                                (ESeq (EFork (ERd (EVar "c")))
                                      (ESeq (EFork (EWr (EInt 0)
                                                        (EVar "c")))
                                            (EFork (EWr (EInt 1)
                                                        (EVar "c")))))))
              , CExpr (EApp (EVar "GetBit")
                            (EInt 1))
              ]
      )
    , ( "product and list types"
      , "myzip :: [Int] -> [Bool] -> [(Int, Bool)]"
      , Right [ CTySig "myzip" (TArrow (TList TInt)
                                       (TArrow (TList TBool)
                                               (TList (TProd [TInt, TBool]))))
              ]
      )
    ]

pmTests :: TestTree
pmTests =
    testGroup "Pattern match tests" $ mkpmTests

mkpmTests = map f pmExamples
  where f (str, v, p, env) = testCase (printf "pattern match %s" str) $
                             assertEqual "" (getBinds p v) env
                             
pmExamples =
    [ ( "tuple w/ vars"
      , VTuple [VInt 1, VBool True]
      , PTuple [PVar "x", PVar "y"]
      , Just [("x", VInt 1), ("y", VBool True)]
      )
    , ( "nested tuple w/ vars"
      , VTuple [VInt 1, VTuple [VInt 2, VInt 3], VBool True]
      , PTuple [PVar "x", PTuple [PVar "y", PWildcard], PVar "z"]
      , Just [("x", VInt 1), ("y", VInt 2), ("z", VBool True)]
      )
    , ( "tuple w/ failure"
      , VTuple [VInt 1, VInt 2, VInt 3]
      , PTuple [PWildcard, PVar "x", PInt 4]
      , Nothing
      )
    , ( "tuple w/ wildcards"
      , VTuple [VInt 1, VInt 2, VInt 3]
      , PTuple [PWildcard, PWildcard, PWildcard]
      , Just []
      )
    , ( "cons success"
      , VList [VInt 1, VInt 2, VInt 3]
      , PCons (PVar "hd") (PVar "tl")
      , Just [("hd", VInt 1), ("tl", VList [VInt 2, VInt 3])]
      )
    , ( "cons success w/ nil list"
      , VList [VInt 1]
      , PCons (PVar "hd") (PList [])
      , Just [("hd", VInt 1)]
      )
    , ( "cons success w/ nil list 2"
      , VList [VInt 1]
      , PCons (PVar "hd") (PVar "tl")
      , Just [("hd", VInt 1), ("tl", VList [])]
      )
    , ( "cons fail"
      , VList [VInt 1, VInt 2]
      , PCons (PVar "hd") (PList [])
      , Nothing
      )
    , ( "cons fail on nil"
      , VList []
      , PCons (PVar "hd") (PVar "tl")
      , Nothing
      )
    , ( "double cons"
      , VList [VInt 1, VInt 2]
      , PCons (PVar "a") (PCons (PVar "b") (PVar "c"))
      , Just [("a", VInt 1), ("b", VInt 2), ("c", VList [])]
      )
    ]

{-execTests :: TestTree
execTests =
    testGroup "Execution tests" $ mkExecTests

mkExecTests = map f execExamples
  where f (str, src, out) = testCase (printf "execute %s" str) $
                            assertEqual "" (run <$> parser src) out

-- TODO: Move these to files                            
execExamples =
    [ ( "factorial"
      , "let f n = if n == 0 then 1 else n * f (n - 1) in f 6"
      , Right $ Just $ VInt 720
      )
    , ( "factorial w/ pattern matching"
      , "let f n = match n with | 0 => 1 | _ => n * f (n - 1) in f 6"
      , Right $ Just $ VInt 720
      )
    , ( "slow fib"
      , "  let fib n = if n < 1 \
         \             then 0 \
         \             else if n < 3 \
         \                  then 1 \
         \                  else fib (n - 2) + fib (n - 1) \
         \ in fib 5"
      , Right $ Just $ VInt 5
      )
    , ( "slow fib w/ pattern matching for the lols"
      , "let fib n = match n with \
         \           | n when n < 1 => 0 \
         \           | n when n < 3 => 1 \
         \           | n => fib (n - 2) + fib (n - 1) in fib 6"
      , Right $ Just $ VInt 8
      )
    ]
-}
