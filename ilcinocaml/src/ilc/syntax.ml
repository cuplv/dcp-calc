(* Abstract syntax *)

type name = string

(* Types *)
type ty = 
    | TNat
    | TBool
    | TArrow of ty * ty

type expr =
    | Name of name
    | Int of int
    | Bool of bool
    (* Binary ops *)
    | Plus of expr * expr
    | Minus of expr * expr
    | Times of expr * expr
    | Divide of expr * expr
    | Mod of expr * expr
    | Less of expr * expr
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
    (* Seq *)
    | Seq of expr * expr

type process =
    | Process of expr
