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
tests = testGroup "Tests" [parserTests, pmTests, execTests]

parserTests :: TestTree
parserTests =
    testGroup "Parser tests" $ makeParserTests

makeParserTests = map f parserExamples
  where f (str, src, ast) = testCase (printf "parse %s" str) $
                            assertEqual "" (parser src) (ast)
                           
parserExamples =
    [ ( "lambda"
      , "lam x . x + x"
      , Right [ ("it", (ELam (PVar "x")
                             (EBin Add (EVar "x")
                                       (EVar "x"))))
              ]
      )
    , ( "allocate channel then write"
      , "nu c . wr 1 -> c"
      , Right [ ("it", (ENu "c" (EWr (ELit $ LInt 1)
                                (EVar "c"))))
              ]
      )
    , ( "let binding"
      , "let x = 100 in x * 1"
      , Right [ ("it", (ELet (PVar "x")
                             (ELit $ LInt 100)
                             (EBin Mul (EVar "x")
                                       (ELit $ LInt 1))))
              ]
      )
    , ( "let binding w/ tuple matching"
      , "let (x, y) = (1, 2) in x + y"
      , Right [ ("it", (ELet (PTuple [PVar "x", PVar "y"])
                             (ETuple [ELit $ LInt 1, ELit $ LInt 2])
                             (EBin Add (EVar "x")
                                       (EVar "y"))))
              ]
      )
    , ( "let binding w/ unit and function application"
      , "let () = \"whatever\" in double 2"
      , Right [ ("it", (ELet (PUnit)
                             (ELit $ LString "whatever")
                             (EApp (EVar "double")
                                   (ELit $ LInt 2))))
              ]
      )
    , ( "sequencing let bindings"
      , "let x = 1 in x; let y = 1 in y"
      , Right [ ("it", (ELet (PVar "x")
                             (ELit $ LInt 1)
                             (ESeq (EVar "x")
                                   (ELet (PVar "y")
                                         (ELit $ LInt 1)
                                         (EVar "y")))))
              ]
      )
    , ( "nested let bindings"
      , "let x = 1 in let y = 2 in x + y"
      , Right [ ("it", (ELet (PVar "x")
                             (ELit $ LInt 1)
                             (ELet (PVar "y")
                                   (ELit $ LInt 2)
                                   (EBin Add (EVar "x")
                                             (EVar "y")))))
              ]
      )
    , ( "let commands"
      , "let x = 1 let y = 2 let z = x + y"
      , Right [ ("x", ELit $ LInt 1)
              , ("y", ELit $ LInt 2)
              , ("z", EBin Add (EVar "x")
                               (EVar "y"))
              ]
      )
    , ( "let command, let binding, expr command"
      , "let z = let x = 1 in 2 * x let y = 1;; \"foo\""
      , Right [ ("z", ELet (PVar "x")
                           (ELit $ LInt 1)
                           (EBin Mul (ELit $ LInt 2)
                                     (EVar "x")))
              , ("y", ELit $ LInt 1)
              , ("it", (ELit $ LString "foo"))
              ]
      )
    , ( "expr commands and sequencing"
      , "1 ; 2 ;; 3 ; 4"
      , Right [ ("it", (ESeq (ELit $ LInt 1)
                             (ELit $ LInt 2)))
              , ("it", (ESeq (ELit $ LInt 3)
                             (ELit $ LInt 4)))
              ]
      )
    , ( "pattern matching"
      , "match b with | 0 => \"zero\" | 1 => \"one\""
      , Right [ ("it", (EMatch (EVar "b")
                               ([ (PInt 0
                                  , ELit $ LBool True
                                  , ELit $ LString "zero")
                                , (PInt 1
                                  , ELit $ LBool True
                                  , ELit $ LString "one")
                                ])))
              ]
      )
    , ( "let binding w/ assign"
      , "let x = 1 ; let y := 1 in x + y"
      , Right [ ("it", (ELet (PVar "x")
                             (ESeq (ELit $ LInt 1)
                                   (EAssign "y"
                                            (ELit $ LInt 1)))
                             (EBin Add (EVar "x")
                                       (EVar "y"))))
              ]
      )
    , ( "ref and deref"
      , "let a = ref 1 ;; let b := @ a"
      , Right [ ("a", ERef (ELit $ LInt 1))
              , ("it", EAssign "b"
                               (EDeref (EVar "a")))
              ]
      )
    , ( "let binding w/ sequencing and assign"
      , "let a = 1 ; let b := 1 in b"
      , Right [ ("it", (ELet (PVar "a")
                             (ESeq (ELit $ LInt 1)
                                   (EAssign "b"
                                            (ELit $ LInt 1)))
                             (EVar "b")))
              ]
      )
    , ( "cons pattern matching"
      , "match a with | [] => 0 | x:xs => 1"
      , Right [ ("it", (EMatch (EVar "a")
                               ([ ( PList []
                                  , ELit $ LBool True
                                  , ELit $ LInt 0)
                                , ( PCons (PVar "x")
                                          (PVar "xs")
                                  , ELit $ LBool True
                                  , ELit $ LInt 1)
                                ])))
              ]
      )
    , ( "pattern matching with guards"
      , "match b with | 0 when 0 < 1 => 0 | 1 when true => 1"
      , Right [ ("it", (EMatch (EVar "b")
                               ([ ( PInt 0
                                  , EBin Lt (ELit $ LInt 0)
                                            (ELit $ LInt 1)
                                  , ELit $ LInt 0)
                                , ( PInt 1
                                  , ELit $ LBool True
                                  , ELit $ LInt 1)
                                ])))
              ]
      )
    {-, ( "plus function w/ type signature"
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
      )-}
    , ( "GetBit"
      , "let GetBit = lam x . nu c . |> (rd c) ; |> (wr 0 -> c) ; |> (wr 1 -> c) GetBit 1"
      , Right [ ( "GetBit"
                , (ELam (PVar "x")
                        (ENu "c" (ESeq (EFork (ERd (EVar "c")))
                                       (ESeq (EFork (EWr (ELit $ LInt 0)
                                                         (EVar "c")))
                                             (EFork (EWr (ELit $ LInt 1)
                                                         (EVar "c"))))))))
              , ("it", EApp (EVar "GetBit")
                            (ELit $ LInt 1))
              ]
      )
    {-, ( "product and list types"
      , "myzip :: [Int] -> [Bool] -> [(Int, Bool)]"
      , Right [ CTySig "myzip" (TArrow (TList TInt)
                                       (TArrow (TList TBool)
                                               (TList (TProd [TInt, TBool]))))
              ]
      )-}
    , ( "factorial"
      , "let f n = if n == 0 then 1 else n * f (n - 1)"
      , Right [ ( "f"
                , ELam (PVar "n")
                       (EIf (EBin Eql (EVar "n") (ELit $ LInt 0))
                            (ELit $ LInt 1)
                            (EBin Mul (EVar "n")
                                      (EApp (EVar "f")
                                            (EBin Sub (EVar "n")
                                                      (ELit $ LInt 1))))))
              ]
      )
    , ( "factorial2"
      , "let f n = if n == 0 then 1 else n * f (n - 1) in f 6"
      , Right [ ( "it"
                , EFun "f"
                       (ELam (PVar "n")
                            (EIf (EBin Eql (EVar "n") (ELit $ LInt 0))
                                 (ELit $ LInt 1)
                                 (EBin Mul (EVar "n")
                                           (EApp (EVar "f")
                                                 (EBin Sub (EVar "n")
                                                           (ELit $ LInt 1))))))
                       (EApp (EVar "f")
                             (ELit $ LInt 6)))
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

{-tyCheckTests :: TestTree
tyCheckTests =
    testGroup "Type check tests" $ makeTyCheckTests

makeTyCheckTests = map f tyCheckExamples
  where f (str, src, ast) = testCase (printf "parse %s" str) $
                            assertEqual "" (parser src) (ast)
                           
tyCheckExamples =-}

execTests :: TestTree
execTests =
    testGroup "Execution tests" $ mkExecTests

testOutEqual src out = do
    case parser src of
        Left err   -> print err
        Right cmds -> do v <- exec cmds
                         assertEqual "" v out
    

mkExecTests = map f execExamples
  where f (str, src, out) = testCase (printf "execute %s" str) $
                            testOutEqual src out

execExamples =
    [ ( "factorial"
      , "let f n = if n == 0 then 1 else n * f (n - 1) in f 6"
      , VInt 720
      )
    , ( "factorial w/ pattern matching"
      , "let f n = match n with | 0 => 1 | _ => n * f (n - 1) in f 6"
      , VInt 720
      )
    , ( "slow fib"
      , "  let fib n = if n < 1 \
         \             then 0 \
         \             else if n < 3 \
         \                  then 1 \
         \                  else fib (n - 2) + fib (n - 1) \
         \ in fib 5"
      , VInt 5
      )
    , ( "slow fib w/ pattern matching for the lols"
      , "let fib n = match n with \
         \           | n when n < 1 => 0 \
         \           | n when n < 3 => 1 \
         \           | n => fib (n - 2) + fib (n - 1) in fib 6"
      , VInt 8
      )
    ]
