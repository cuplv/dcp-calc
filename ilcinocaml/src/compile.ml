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
    (* Identifier, constants, values *)
    | Name x ->  [IVar x]
    | Tag x ->  [ITag x]
    | Int n -> [IInt n]
    | Bool b -> [IBool b]
    | String s -> [IString s]
    | List es ->
        [IStartL] @ List.fold_left (fun acc e -> acc @ (compile e))
        [] es @ [IEndL]
    | Tuple es ->
        [IStartT] @ List.fold_left (fun acc e -> acc @ (compile e))
        [] es @ [IEndT]
    | Wildcard -> [IWCard]

    (* Arithmetic operators *)
    | Plus (e1, e2) -> (compile e1) @ (compile e2) @ [IAdd]
    | Minus (e1, e2) -> (compile e1) @ (compile e2) @ [ISub]
    | Times (e1, e2) -> (compile e1) @ (compile e2) @ [IMult]
    | Divide (e1, e2) -> (compile e1) @ (compile e2) @ [IDiv]
    | Mod (e1, e2) -> (compile e1) @ (compile e2) @ [IMod]

    (* Logical operators *)
    | Or (e1, e2) -> (compile e1) @ (compile e2) @ [IOr]
    | And (e1, e2) -> (compile e1) @ (compile e2) @ [IAnd]
    | Not e -> (compile e)  @ [INot]

    (* Relations *)
    | Lt (e1, e2) -> (compile e1) @ (compile e2) @ [ILt]
    | Gt (e1, e2) -> (compile e1) @ (compile e2) @ [IGt]
    | Leq (e1, e2) -> (compile e1) @ (compile e2) @ [ILeq]
    | Geq (e1, e2) -> (compile e1) @ (compile e2) @ [IGeq]
    | Eq (e1, e2) -> (compile e1) @ (compile e2) @ [IEq]
    | Neq (e1, e2) -> (compile e1) @ (compile e2) @ [INeq]

    (* Let *)
    | Let (x, e1, e2) -> (compile e1) @ [ILet x] @ (compile e2)
    | LetRec (x, e1, e2) ->
        [IThunk (force_thunks x (compile e1))] @
        [ILet x] @ (force_thunks x (compile e2))
    | LetP (p, e1, e2) -> (compile e1) @
        [IStartT] @ List.fold_left (fun acc e ->
            match e with
            | IVar x -> IVarP x :: acc
            | instr -> instr :: acc)
        [] (List.rev (List.fold_left (fun acc e -> acc @ (compile e))
        [] p)) @ [IEndT] @ [ILetP] @ (compile e2)
    (* Conditionals *)
    | IfTE (e1, e2, e3) ->
        (compile e1) @ [IBranch (compile e2, compile e3)]
    | IfT (e1, e2) -> (compile e1) @ [ICond (compile e2)]

    (* Lambda *)
    | Lam (x, e) -> [IClosure ("anon", x, compile e @ [IPopEnv])]
    | App (e1, e2) -> (compile e1) @ (compile e2) @ [ICall]

    (* Pi *)
    | Wr (e, x) -> (compile e) @ [IWr (MHole, x)]
    | WrSpec (ts, e, x) -> (* Syntactic sugar transformation *)
        if List.mem Delay ts then
            compile (Seq(Wr(e,"f2a"),LetP([Tag("'ok")],Rd("a2f"),Wr(e,x))))
        else []
    | Rd x -> [IRd x]
    | RdBind (x1, x2) -> [IRdBind (x1, x2)]
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
    | Repl e -> [IRepl (compile e)]
    | Seq (e1, e2) -> (compile e1) @ (compile e2)

    (* Laziness *)
    | Thunk e -> [IThunk (compile e)]
    | Force e -> (compile e) @ [IForce]

    (* Built-in functions *)
    | Fst e -> (compile e) @ [IFst]
    | Snd e -> (compile e) @ [ISnd]
    | Rand -> [IRand]
    | Show e -> (compile e) @ [IShow]
    | Cons (e1, e2) -> (compile e1) @ (compile e2) @ [ICons]
    | Concat (e1, e2) -> (compile e1) @ (compile e2) @ [IConcat]
    | Lookup (e1, e2) -> (compile e1) @ (compile e2) @[ILookup]
    | _ -> error "not implemented yet"
