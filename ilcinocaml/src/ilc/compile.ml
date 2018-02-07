(* -------------------------------------------------------------------------- *)
(* Compile to stack-based language *)

open Machine
open Syntax

exception Compilation_error

let pid_counter = ref 0

let convert_to_choice pid cid = function
    | instr :: instrs -> IChoice(pid, cid, instr) :: instrs
    | _ -> raise (Compilation_error)

let add_force x = function
    | IVar x' when x=x' -> [IVar x; IForce]
    | instr -> [instr]

let force_thunks x = List.fold_left (fun acc instr ->
    acc @ (add_force x instr)) []

let rec compile = function
    | Name x ->  [IVar x]
    | Int n -> [IInt n]
    | Bool b -> [IBool b]
    | String s -> [IString s]
    | Plus (e1, e2) -> (compile e1) @ (compile e2) @ [IAdd]
    | Minus (e1, e2) -> (compile e1) @ (compile e2) @ [ISub]
    | Times (e1, e2) -> (compile e1) @ (compile e2) @ [IMult]
    | Divide (e1, e2) -> (compile e1) @ (compile e2) @ [IDiv]
    | Mod (e1, e2) -> (compile e1) @ (compile e2) @ [IMod]
    | Lt (e1, e2) -> (compile e1) @ (compile e2) @ [ILt]
    | Gt (e1, e2) -> (compile e1) @ (compile e2) @ [IGt]
    | Leq (e1, e2) -> (compile e1) @ (compile e2) @ [ILeq]
    | Geq (e1, e2) -> (compile e1) @ (compile e2) @ [IGeq]
    | Or (e1, e2) -> (compile e1) @ (compile e2) @ [IOr]
    | And (e1, e2) -> (compile e1) @ (compile e2) @ [IAnd]
    | Not e -> (compile e)  @ [INot]
    | Eq (e1, e2) -> (compile e1) @ (compile e2) @ [IEq]
    | Neq (e1, e2) -> (compile e1) @ (compile e2) @ [INeq]
    | IfTE (e1, e2, e3) ->
        (compile e1) @ [IBranch (compile e2, compile e3)]
    | IfT (e1, e2) -> (compile e1) @ [ICond (compile e2)]
    | Thunk e -> [IThunk (compile e)]
    | Force e -> (compile e) @ [IForce]
    | Let (x, e1, e2) -> (compile e1) @ [ILet x] @ (compile e2)
    | LetRec (x, e1, e2) ->
        [IThunk (force_thunks x (compile e1))] @
        [ILet x] @ (force_thunks x (compile e2))
    | LetP (p, e1, e2) -> (compile e1) @
        [IStartL] @ List.fold_left (fun acc e ->
            match e with
            | IVar x -> IVarP x :: acc
            | instr -> instr :: acc)
        [] (List.rev (List.fold_left (fun acc e -> acc @ (compile e))
        [] p)) @ [IEndL] @ [ILetP] @ (compile e2)
    | Lam (x, e) -> [IClosure ("anon", x, compile e @ [IPopEnv])]
    | App (e1, e2) -> (compile e1) @ (compile e2) @ [ICall]
    | Nu (x, e) -> [INu x] @ (compile e)
    | ParComp (e1, e2) ->
        let pid = !pid_counter in
        pid_counter := pid + 2; [IStartP pid] @
        (compile e1) @ [IEndP pid; IStartP (succ pid)] @
        (compile e2) @ [IEndP (succ pid)]
    | ParLeft (e1, e2) ->
        let pid = !pid_counter in
        pid_counter := pid + 2; [IStartP pid] @
        (compile e1) @ [IEndP pid; IStartP (succ pid)] @
        (compile e2) @ [IEndP (succ pid); IHole (succ pid)]
    | Choice (e1, e2) -> (* TODO: Don't think this works for nested choices *)
        let pid = !pid_counter in
        pid_counter := pid + 2; [IStartP pid] @
        convert_to_choice pid 0 (compile e1) @ [IEndP pid; IStartP (succ pid)] @
        convert_to_choice pid 1 (compile e2) @ [IEndP (succ pid); IHole (succ pid)]
    | Wr (e, x) -> (compile e) @ [IWr (MHole, x)]
    | Rd x -> [IRd x]
    | RdBind (x1, x2) -> [IRdBind (x1, x2)]
    | Seq (e1, e2) -> (compile e1) @ (compile e2)
    | List es ->
        [IStartL] @ List.fold_left (fun acc e -> acc @ (compile e))
        [] es @ [IEndL]
    | Cons (e1, e2) -> (compile e1) @ (compile e2) @ [ICons]
    | Concat (e1, e2) -> (compile e1) @ (compile e2) @ [IConcat]
    | Tuple es ->
        [IStartT] @ List.fold_left (fun acc e -> acc @ (compile e))
        [] es @ [IEndT]
    | Fst e -> (compile e) @ [IFst]
    | Snd e -> (compile e) @ [ISnd]
    | Repl e -> [IRepl (compile e)]
    | Rand -> [IRand]
    | Show e -> (compile e) @ [IShow]
    | Lookup (e1, e2) -> (compile e1) @ (compile e2) @[IShow]
    | _ -> raise (Compilation_error)
