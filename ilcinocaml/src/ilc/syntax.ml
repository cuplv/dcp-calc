(* -------------------------------------------------------------------------- *)
(* Abstract syntax tree *)

type name = string

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
    | LetRec of name * expr * expr
    | LetP of name list * expr * expr
    (* App *)
    | App of expr * expr
    (* Lam *)
    | Lam of name * expr
    (* Write to channel *)
    | Wr of expr * name
    (* Read from channel *)
    | Rd of name
    | RdBind of name * name
    (* Restriction *)
    | Nu of name list * expr
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
    | Concat of expr * expr
    (* Pair *)
    | Fst of expr
    | Snd of expr
    (* Built-in functions *)
    | Rand
    | Show of expr
    | Repl of expr
    | Lookup of expr * expr

type process =
    | Process of expr

(* -------------------------------------------------------------------------- *)
(* Printing *)

let name_to_str = function
    | x -> "Name(" ^ x ^ ")"

let str_of_list f es = 
    let rec to_str acc = function
        | [] -> acc
        | [e] -> acc ^ (f e)
        | e :: es -> to_str (acc ^ (f e) ^ ",") es
    in
    to_str "" es

let string_of_expr e =
    let rec to_str e = 
        match e with
        | Name x -> name_to_str x
        | Int n -> "Int(" ^ string_of_int n ^ ")"
        | Bool b -> "Bool(" ^ string_of_bool b ^ ")"
        | String s -> "String(" ^ s ^ ")"
        | List es -> "List(" ^ str_of_list to_str es ^ ")"
        | Tuple es -> "Tuple(" ^ str_of_list to_str es ^ ")"
        | Plus (e1, e2) -> "Plus(" ^ to_str e1 ^ "," ^ to_str e2 ^ ")"
        | Minus (e1, e2) -> "Minus(" ^ to_str e1 ^ "," ^ to_str e2 ^ ")"
        | Times (e1, e2) -> "Times(" ^ to_str e1 ^ "," ^ to_str e2 ^ ")"
        | Divide (e1, e2) -> "Divide(" ^ to_str e1 ^ "," ^ to_str e2 ^ ")"
        | Mod (e1, e2) -> "Mod(" ^ to_str e1 ^ "," ^ to_str e2 ^ ")"
        | Lt (e1, e2) -> "Lt(" ^ to_str e1 ^ "," ^ to_str e2 ^ ")"
        | Gt (e1, e2) -> "Gt(" ^ to_str e1 ^ "," ^ to_str e2 ^ ")"
        | Leq (e1, e2) -> "Leq(" ^ to_str e1 ^ "," ^ to_str e2 ^ ")"
        | Geq (e1, e2) -> "Geq(" ^ to_str e1 ^ "," ^ to_str e2 ^ ")"
        | Or (e1, e2) -> "Or(" ^ to_str e1 ^ "," ^ to_str e2 ^ ")"
        | And (e1, e2) -> "And(" ^ to_str e1 ^ "," ^ to_str e2 ^ ")"
        | Not e1 -> "Not(" ^ to_str e1 ^ ")"
        | Eq (e1, e2) -> "Eq(" ^ to_str e1 ^ "," ^ to_str e2 ^ ")"
        | Neq (e1, e2) -> "Neq(" ^ to_str e1 ^ "," ^ to_str e2 ^ ")"
        | IfTE (e1, e2, e3) -> "IfTE(" ^ to_str e1 ^ "," ^ 
          to_str e2 ^ "," ^ to_str e3 ^ ")"
        | IfT (e1, e2) -> "IfT(" ^ to_str e1 ^ "," ^ 
          to_str e2 ^ ")"
        | Thunk e1 -> "Thunk(" ^ to_str e1 ^ ")"
        | Force e1 -> "Force(" ^ to_str e1 ^ ")"
        | Let (x, e1, e2) -> "Let(" ^ name_to_str x ^ "," ^ 
          to_str e1 ^ "," ^ to_str e2 ^ ")"
        | LetRec (x, e1, e2) -> "LetRec(" ^ name_to_str x ^ "," ^ 
          to_str e1 ^ "," ^ to_str e2 ^ ")"
        | LetP (p, e1, e2) -> "LetP(" ^ "(" ^ str_of_list (fun x -> x) p ^ ")," ^ 
          to_str e1 ^ "," ^ to_str e2 ^ ")"
        | App (e1, e2) -> "App(" ^ to_str e1 ^ "," ^ to_str e2 ^ ")"
        | Lam (x, e) -> "Lam(" ^ name_to_str x ^ "," ^ to_str e ^ ")"
        | Wr (e, x) -> "Wr(" ^ to_str e ^ "," ^ name_to_str x ^ ")"
        | RdBind (x1, x2) -> "RdBind(" ^ name_to_str x1 ^ "," ^ name_to_str x2 ^ ")"
        | Rd x -> "Rd(" ^ name_to_str x ^ ")"
        | Nu (x, e) -> "Nu(" ^ str_of_list name_to_str x ^ "," ^ to_str e ^ ")"
        | ParComp (e1, e2) -> "ParComp(" ^ to_str e1 ^ "," ^ to_str e2 ^ ")"
        | ParLeft (e1, e2) -> "ParLeft(" ^ to_str e1 ^ "," ^ to_str e2 ^ ")"
        | Choice (e1, e2) -> "Choice(" ^ to_str e1 ^ "," ^ to_str e2 ^ ")"
        | Seq (e1, e2) -> "Seq(" ^ to_str e1 ^ "," ^ to_str e2 ^ ")"
        | CommaSep (e1, e2) -> "CommaSep(" ^ to_str e1 ^ "," ^ to_str e2 ^ ")"
        | Cons (e1, e2) -> "Cons(" ^ to_str e1 ^ "," ^ to_str e2 ^ ")"
        | Concat (e1, e2) -> "Concat(" ^ to_str e1 ^ "," ^ to_str e2 ^ ")"
        | Fst e -> "Fst(" ^ to_str e ^ ")"
        | Snd e -> "Snd(" ^ to_str e ^ ")"
        | Repl e -> "Repl(" ^ to_str e ^ ")"
        | Rand -> "Rand()"
        | Show e -> "Show(" ^ to_str e ^ ")"
        | Lookup (e1, e2) -> "Lookup(" ^ to_str e1 ^ "," ^ to_str e2 ^ ")"
    in to_str e
