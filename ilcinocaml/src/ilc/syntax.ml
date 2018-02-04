(* Abstract syntax *)

type name = string
type chan = string

(* Types *)
type ty = 
    | TNat
    | TBool
    | TString of string
    | TArrow of ty * ty

type expr =
    | Name of name
    | Int of int
    | Bool of bool
    | String of string
    | List of expr list
    | Tuple of expr list
    (* Binary ops *)
    | Plus of expr * expr
    | Minus of expr * expr
    | Times of expr * expr
    | Divide of expr * expr
    | Mod of expr * expr
    | Lt of expr * expr
    | Gt of expr * expr
    | Leq of expr * expr
    | Geq of expr * expr
    | Or of expr * expr
    | And of expr * expr
    | Not of expr
    | Eq of expr * expr
    | Neq of expr * expr
    (* Conditionals *)
    | IfTE of expr * expr * expr
    | IfT of expr * expr
    (* Laziness *)
    | Thunk of expr
    | Force of expr
    (* Let *)
    | Let of name * expr * expr
    | LetP of name list * expr * expr
    (* App *)
    | App of expr * expr
    (* Lam *)
    | Lam of name * expr
    (* Write to channel *)
    | Wr of expr * name
    (* Read from channel *)
    | Rd of name * name
    (* Restriction *)
    | Nu of name * expr
    (* Parallel composition *)
    | ParComp of expr * expr
    (* Parallel left *)
    | ParLeft of expr * expr
    (* Parallel choice *)
    | Choice of expr * expr
    (* Seq *)
    | Seq of expr * expr
    (* Lists *)
    | CommaSep of expr * expr
    | Cons of expr * expr
    (* Pair *)
    | Fst of expr
    | Snd of expr
    | Rand
    | Repl of expr

type process =
    | Process of expr
