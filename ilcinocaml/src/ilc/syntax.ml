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
    (* Arithmetic *)
    | Plus of expr * expr
    | Minus of expr * expr
    | Times of expr * expr
    | Divide of expr * expr
    | Mod of expr * expr
    (* Comparison *)
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
    | Rd of expr * name
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

let name_to_str = function
        | x -> "Name(" ^ x ^ ")"
let string_of_expr e =
    let rec to_str e = 
        match e with
        | Name x -> name_to_str x
        | Int n -> "Int(" ^ string_of_int n ^ ")"
        | Bool b -> "Bool(" ^ string_of_bool b ^ ")"
        | Plus (e1, e2) -> "Plus(" ^ to_str e1 ^ "," ^ to_str e2 ^ ")"
        | Minus (e1, e2) -> "Minus(" ^ to_str e1 ^ "," ^ to_str e2 ^ ")"
        | Times (e1, e2) -> "Times(" ^ to_str e1 ^ "," ^ to_str e2 ^ ")"
        | Divide (e1, e2) -> "Divide(" ^ to_str e1 ^ "," ^ to_str e2 ^ ")"
        | Mod (e1, e2) -> "Mod(" ^ to_str e1 ^ "," ^ to_str e2 ^ ")"
        | Less (e1, e2) -> "Less(" ^ to_str e1 ^ "," ^ to_str e2 ^ ")"
        | If (e1, e2, e3) -> "If(" ^ to_str e1 ^ "," ^ 
          to_str e2 ^ "," ^ to_str e3 ^ ")"
        | Let (x, e1, e2) -> "Let(" ^ name_to_str x ^ "," ^ 
          to_str e1 ^ "," ^ to_str e2 ^ ")"
        | App (e1, e2) -> "App(" ^ to_str e1 ^ "," ^ to_str e2 ^ ")"
        | Lam (x, e) -> "Lam(" ^ name_to_str x ^ "," ^ to_str e ^ ")"
        | Wr (e, x) -> "Wr(" ^ to_str e ^ "," ^ name_to_str x ^ ")"
        | Rd (e, x) -> "Rd(" ^ to_str e ^ "," ^ name_to_str x ^ ")"
        | Nu (x, e) -> "Nu(" ^ name_to_str x ^ "," ^ to_str e ^ ")"
        | ParComp (e1, e2) -> "ParComp(" ^ to_str e1 ^ "," ^ to_str e2 ^ ")"
        | ParLeft (e1, e2) -> "ParLeft(" ^ to_str e1 ^ "," ^ to_str e2 ^ ")"
        | Seq (e1, e2) -> "Seq(" ^ to_str e1 ^ "," ^ to_str e2 ^ ")"
    in to_str e
