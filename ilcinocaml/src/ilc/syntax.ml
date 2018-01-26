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
    | x -> Printf.sprintf "Name(%s)" x

let string_of_expr e =
    let rec to_str e = 
        match e with
        | Name x -> name_to_str x
        | Int n -> Printf.sprintf "Int(%d)" n
        | Bool b -> Printf.sprintf "Bool(%b)" b
        | Plus (e1, e2) -> Printf.sprintf "Plus(%s,%s)" (to_str e1) (to_str e2)
        | Minus (e1, e2) -> Printf.sprintf "Minus(%s,%s)" (to_str e1) (to_str e2)
        | Times (e1, e2) -> Printf.sprintf "Times(%s,%s)" (to_str e1) (to_str e2)
        | Divide (e1, e2) -> Printf.sprintf "Divide(%s,%s)" (to_str e1) (to_str e2)
        | Mod (e1, e2) -> Printf.sprintf "Mod(%s,%s)" (to_str e1) (to_str e2)
        | Less (e1, e2) -> Printf.sprintf "Less(%s,%s)" (to_str e1) (to_str e2)
        | If (e1, e2, e3) -> Printf.sprintf "If(%s,%s,%s)" (to_str e1) (to_str e2) (to_str e3)
        | Let (x, e1, e2) -> Printf.sprintf "Let(%s,%s,%s)" (name_to_str x) (to_str e1) (to_str e2)
        | App (e1, e2) -> Printf.sprintf "App(%s,%s)" (to_str e1) (to_str e2)
        | Lam (x, e) -> Printf.sprintf "Lam(%s,%s)" (name_to_str x) (to_str e)
        | Wr (e, x) -> Printf.sprintf "Wr(%s,%s)" (to_str e) (name_to_str x)
        | Rd (e, x) -> Printf.sprintf "Rd(%s,%s)" (to_str e) (name_to_str x)
        | Nu (x, e) -> Printf.sprintf "Nu(%s,%s)" (name_to_str x) (to_str e)
        | ParComp (e1, e2) -> Printf.sprintf "ParComp(%s,%s)" (to_str e1) (to_str e2)
        | ParLeft (e1, e2) -> Printf.sprintf "ParLeft(%s,%s)" (to_str e1) (to_str e2)
        | Seq (e1, e2) -> Printf.sprintf "Seq(%s,%s)" (to_str e1) (to_str e2)
    in to_str e
