open Compile
open Machine
open Printf
open Syntax

(* Convert abstract syntax tree into string *)
let name_to_str = function
    | x -> "Name(" ^ x ^ ")"

let string_of_expr e =
    let rec to_str e = 
        match e with
        | Name x -> name_to_str x
        | Int n -> "Int(" ^ string_of_int n ^ ")"
        | Bool b -> "Bool(" ^ string_of_bool b ^ ")"
        | String s -> "String(" ^ s ^ ")"
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
        | If (e1, e2, e3) -> "If(" ^ to_str e1 ^ "," ^ 
          to_str e2 ^ "," ^ to_str e3 ^ ")"
        | Thunk e1 -> "Thunk(" ^ to_str e1 ^ ")"
        | Force e1 -> "Force(" ^ to_str e1 ^ ")"
        | Let (x, e1, e2) -> "Let(" ^ name_to_str x ^ "," ^ 
          to_str e1 ^ "," ^ to_str e2 ^ ")"
        | App (e1, e2) -> "App(" ^ to_str e1 ^ "," ^ to_str e2 ^ ")"
        | Lam (x, e) -> "Lam(" ^ name_to_str x ^ "," ^ to_str e ^ ")"
        | Wr (e, x) -> "Wr(" ^ to_str e ^ "," ^ name_to_str x ^ ")"
        | Rd (x1, x2) -> "Rd(" ^ name_to_str x1 ^ "," ^ name_to_str x2 ^ ")"
        | Nu (x, e) -> "Nu(" ^ name_to_str x ^ "," ^ to_str e ^ ")"
        | ParComp (e1, e2) -> "ParComp(" ^ to_str e1 ^ "," ^ to_str e2 ^ ")"
        | ParLeft (e1, e2) -> "ParLeft(" ^ to_str e1 ^ "," ^ to_str e2 ^ ")"
        | Choice (e1, e2) -> "Choice(" ^ to_str e1 ^ "," ^ to_str e2 ^ ")"
        | Seq (e1, e2) -> "Seq(" ^ to_str e1 ^ "," ^ to_str e2 ^ ")"
    in to_str e

(* Convert machine value into string *)
let string_of_mvalue = function
    | MInt n -> string_of_int n
    | MBool b -> string_of_bool b
    | MString s -> s
    | MThunk _ -> "<thunk>"
    | MClosure _ -> "<fun>"
    | MHole -> "hole"

(* Convert instruction into string *)
let rec string_of_instr = function 
    | IVar x -> sprintf "IVar(%s)" x
    | IInt n -> sprintf "IInt(%d)" n
    | IBool b -> sprintf "IBool(%b)" b
    | IString s -> sprintf "IString(%s)" s
    | IAdd -> "IAdd"
    | ISub -> "ISub"
    | IMult -> "IMult"
    | IDiv -> "IDiv"
    | IMod -> "IMod"
    | ILt -> "ILt"
    | IGt -> "IGt"
    | ILeq -> "ILeq"
    | IGeq -> "IGeq"
    | IOr -> "IOr"
    | IAnd -> "IAnd"
    | INot -> "INot"
    | IEq -> "IEq"
    | INeq -> "INeq"
    | IClosure (_, x, f) ->
         sprintf "IClosure(%s)" x 
    | IBranch (f1, f2) ->
         sprintf "IBranch()" 
    | ICall -> "ICall" 
    | IPopEnv -> "IPopEnv"
    | IThunk e -> "IThunk"
    | IForce -> "IForce"
    | ILet x -> sprintf "ILet(%s)" x
    | IWr (v, x) -> sprintf "IWr(%s,%s)" (string_of_mvalue v) x
    | IRd (x1, x2) -> sprintf "IRd(%s,%s)" x1 x2 
    | IStartP n -> sprintf "IStartP(%d)" n
    | IEndP n -> sprintf "IEndP(%d)" n
    | IChoice (p, c, i) -> sprintf "IChoice(%d,%d,%s)" p c (string_of_instr i)
    | IBlock i -> sprintf "IBlock(%s)" (string_of_instr i)
    | ISpawn -> "ISpawn"
    | IHole n -> sprintf "IHole(%d)" n

(* Convert instruction list into string *)
let rec string_of_frame = function
    | [] -> "\n"
    | i::is -> string_of_instr i ^ "\n" ^ string_of_frame is

let rec string_of_stack = function
    | [] -> ""
    | v::vs -> string_of_mvalue v ^ "\n" ^ string_of_stack vs 

let string_of_mapping = function
    | (n, v) -> "(" ^ n ^ "," ^ string_of_mvalue v ^ ")"

let rec string_of_environ = function
    | [] -> ""
    | m::ms -> string_of_mapping m ^ "\n" ^ string_of_environ ms