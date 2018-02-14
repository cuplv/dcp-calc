(* -------------------------------------------------------------------------- *)
(* Compile to stack-based language *)

open Machine
open Syntax

exception Compilation_error

let pid_counter = ref 0
let lst_counter = ref 0

let convert_to_choice pid cid = function
  | instr :: instrs -> IChoice(pid, cid, instr) :: instrs
  | _ -> raise (Compilation_error)

(* TODO: ICond, IClosure? *)      
let rec add_force x = function
  | IVar x' when x=x' -> [IVar x; IForce]
  | IBranch (f1,f2) ->
     let f acc instr = (add_force x instr) @ acc in
     let add_force_branch frm = List.fold_left f [] (List.rev frm) in
     [IBranch (add_force_branch f1, add_force_branch f2)]
  | instr -> [instr]

let force_thunks x = List.fold_left (fun acc instr ->
                         acc @ (add_force x instr)) []

let get_proj f l = List.fold_left (fun acc x -> f x :: acc) [] (List.rev l)

let get_vars p =
  let rec aux acc = function
    | Tuple xs :: rest ->
       (aux [] xs) @ aux acc rest
    | Name x :: rest -> aux (x :: acc) rest
    | ImpName x :: rest -> aux (x :: acc) rest
    | Tag _ :: rest -> aux acc rest
    | [] -> acc
    | _ -> error "unexpected pattern"
  in
  aux [] p

let rec compile = function
  (* Identifier, constants, values *)
  | Name x ->  [IVar x]
  | ImpName x ->  [IImpVar x]
  | Tag x ->  [ITag x]
  | Int n -> [IInt n]
  | Bool b -> [IBool b]
  | String s -> [IString s]
  | List es ->
      let lst_id = !lst_counter in
      incr lst_counter; [IStartL lst_id] @ List.fold_left (fun acc e -> acc @ (compile e))
      [] es @ [IEndL lst_id]
  | Set es ->
      let lst_id = !lst_counter in
      incr lst_counter; [IStartS lst_id] @ List.fold_left (fun acc e -> acc @ (compile e))
      [] es @ [IEndS lst_id]
  | Tuple es ->
      let lst_id = !lst_counter in
      incr lst_counter; [IStartT lst_id] @ List.fold_left (fun acc e -> acc @ (compile e))
      [] es @ [IEndT lst_id]
  | Wildcard -> [IWCard]
  | Unit -> [IUnit]
  
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
  | Let (x, e1, e2) -> (compile e1) @ [ILet x] @ (compile e2) @ [IUnscope [x]]
  | LetRec (x, e1, e2) ->
      [IThunk (force_thunks x (compile e1))] @
      [ILet x] @ (force_thunks x (compile e2))
  | LetP (p, e1, e2) ->
      let lst_id = !lst_counter in
      incr lst_counter; (compile e1) @
      [IStartT lst_id] @ List.fold_left (fun acc e ->
          match e with
          | IVar x -> IVarP x :: acc
          | IImpVar x -> IImpVarP x :: acc
          | instr -> instr :: acc)
      [] (List.rev (List.fold_left (fun acc e -> acc @ (compile e))
                                   [] p)) @ [IEndT lst_id] @ [ILetP] @ (compile
  e2) @ [IUnscope (get_vars p)]
  | Match (e, es) ->
     let f acc = function
       | (p, expr) -> (compile p) @ [IMatchCond (compile expr)] @ acc in
     [IStartM] @ (compile e) @ List.fold_left f [] (List.rev es) @ [IEndM]
  | Pattern e ->
     (match e with
     | Tuple e -> List.map (function
                            | IVar x -> IVarP x
                            | IImpVar x -> IImpVarP x
                            | instr -> instr)
                           (compile (Tuple e)) @ [IUnscope (get_vars e)]
     | List [] -> [IEmpListP]
     | Cons(Name hd, Name tl) -> [IListP (hd, tl); IUnscope [hd; tl]]
     | _ -> error "Not implemented")

  (* Conditionals *)
  | IfTE (e1, e2, e3) ->
      (compile e1) @ [IBranch (compile e2, compile e3)]
  | IfT (e1, e2) -> (compile e1) @ [ICond (compile e2)]
  | Req (e1, e2) -> (compile e1) @ [IReq] @ (compile e2)
  
  (* Lambda *)
  | Lam (x, e) ->
     let arg = 
       match x with
       | Name x' -> x'
       | Unit -> "()"
       | _ -> error "invalid argument in lambda" in
     [IClosure ("anon", arg, compile e @ [IPopEnv])]
  | App (e1, e2) -> (compile e1) @ (compile e2) @ [ICall]
  
  (* Pi *)
  | Wr (e, x) -> (compile e) @ [IWr (MHole, x)]
  | Rd x -> [IRd x]
  | RdBind (x1, x2) -> [IRdBind (x1, x2)]
  | Nu (xs, e) -> [INu xs] @ (compile e) @ [IUnscope xs]
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
  | Length e -> (compile e) @ [ILength]
  | Mem (e1, e2) -> (compile e1) @ (compile e2) @[IMem]
  | Union (e1, e2) -> (compile e1) @ (compile e2) @[IUnion]
