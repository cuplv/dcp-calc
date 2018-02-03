open Machine

exception Compilation_error

let pid_counter = ref 0

let make_choice pid cid = function
    | instr :: instrs -> IChoice(pid, cid, instr) :: instrs
    | _ -> raise (Compilation_error)

let rec compile = function
    | Syntax.Name x ->  [IVar x]
    | Syntax.Int n -> [IInt n]
    | Syntax.Bool b -> [IBool b]
    | Syntax.String s -> [IString s]
    | Syntax.Plus (e1, e2) -> (compile e1) @ (compile e2) @ [IAdd]
    | Syntax.Minus (e1, e2) -> (compile e1) @ (compile e2) @ [ISub]
    | Syntax.Times (e1, e2) -> (compile e1) @ (compile e2) @ [IMult]
    | Syntax.Divide (e1, e2) -> (compile e1) @ (compile e2) @ [IDiv]
    | Syntax.Mod (e1, e2) -> (compile e1) @ (compile e2) @ [IMod]
    | Syntax.Lt (e1, e2) -> (compile e1) @ (compile e2) @ [ILt]
    | Syntax.Gt (e1, e2) -> (compile e1) @ (compile e2) @ [IGt]
    | Syntax.Leq (e1, e2) -> (compile e1) @ (compile e2) @ [ILeq]
    | Syntax.Geq (e1, e2) -> (compile e1) @ (compile e2) @ [IGeq]
    | Syntax.Or (e1, e2) -> (compile e1) @ (compile e2) @ [IOr]
    | Syntax.And (e1, e2) -> (compile e1) @ (compile e2) @ [IAnd]
    | Syntax.Not e -> (compile e)  @ [INot]
    | Syntax.Eq (e1, e2) -> (compile e1) @ (compile e2) @ [IEq]
    | Syntax.Neq (e1, e2) -> (compile e1) @ (compile e2) @ [INeq]
    | Syntax.IfTE (e1, e2, e3) -> (compile e1) @ [IBranch (compile e2, compile e3)]
    | Syntax.IfT (e1, e2) -> (compile e1) @ [ICond (compile e2)]
    | Syntax.Thunk e -> [IThunk (compile e)]
    | Syntax.Force e -> (compile e) @ [IForce]
    | Syntax.Let (x, e1, e2) -> (compile e1) @ [ILet x] @ (compile e2)
    | Syntax.Lam (x, e) -> [IClosure ("anon", x, compile e @ [IPopEnv])]
    | Syntax.App (e1, e2) -> (compile e1) @ (compile e2) @ [ICall]
    | Syntax.Nu (x, e) -> (compile e) (* TODO: This is a no-op for now *)
    | Syntax.ParComp (e1, e2) ->
        let pid = !pid_counter in
        pid_counter := pid + 2; [IStartP pid] @
        (compile e1) @ [IEndP pid; IStartP (pid + 1)] @
        (compile e2) @ [IEndP (pid + 1)]
    | Syntax.ParLeft (e1, e2) ->
        let pid = !pid_counter in
        pid_counter := pid + 2; [IStartP pid] @
        (compile e1) @ [IEndP pid; IStartP (pid + 1)] @
        (compile e2) @ [IEndP (pid + 1); IHole (pid+1)]
    | Syntax.Choice (e1, e2) -> (* TODO: Don't think this works for nested choices *)
        let pid = !pid_counter in
        pid_counter := pid + 2; [IStartP pid] @
        make_choice pid 0 (compile e1) @ [IEndP pid; IStartP (pid + 1)] @
        make_choice pid 1 (compile e2) @ [IEndP (pid + 1); IHole (pid+1)]
    | Syntax.Wr (e, x) -> (compile e) @ [IWr (MHole, x)]
    | Syntax.Rd (x1, x2) -> [IRd (x1, x2)]
    | Syntax.Seq (e1, e2) -> (compile e1) @ (compile e2)
    | Syntax.List es -> [IStartL] @ List.fold_left (fun acc e -> acc @ (compile e)) [] es @ [IEndL]
    | Syntax.Cons (e1, e2) -> (compile e1) @ (compile e2) @ [ICons]
    | Syntax.Pair (e1, e2) -> (compile e1) @ (compile e2) @ [IPair]
    | Syntax.Fst e -> (compile e) @ [IFst]
    | Syntax.Snd e -> (compile e) @ [ISnd]
    | Syntax.Repl e -> [IRepl (compile e)]
    | Syntax.Rand -> [IRand]
