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
    | If of expr * expr * expr
    (* Let *)
    | Let of name * expr * expr
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

type process =
    | Process of expr
