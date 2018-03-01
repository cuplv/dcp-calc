(* -------------------------------------------------------------------------- *)
(* Compile to stack-based language *)

open Machine
open Syntax

exception Compilation_error

let pid_counter = ref 0

let convert_to_choice pid cid = function
  | instr :: instrs -> IChoice(pid, cid, instr) :: instrs
  | _ -> raise (Compilation_error)

(* Unsugar letrec *)       
let rec add_force x = function
  | IVar x' when x=x' -> [IVar x; IForce]
  | IBranch (f1,f2) ->
     [IBranch (add_force_branch x f1, add_force_branch x f2)]
  | ICond f1 ->
     [ICond (add_force_branch x f1)]
  | instr -> [instr]
and add_force_branch x frm =
  let f acc instr = (add_force x instr) @ acc in
  List.fold_left f [] (List.rev frm)

let force_thunks x = List.fold_left (fun acc instr ->
                         acc @ (add_force x instr)) []

(* Get variables in pattern *)                 
let get_vars p =
  let rec aux acc = function
    | PatTuple xs :: rest ->
       (aux [] xs) @ aux acc rest
    | PatName x :: rest -> aux (x :: acc) rest
    | PatImpName x :: rest -> aux (x :: acc) rest
    | PatCons (PatName hd, PatName tl) :: rest -> aux ([hd; tl] @ acc) rest
    | PatCons (PatName hd, tl) :: rest -> (aux [] [tl]) @ aux (hd :: acc) rest
    | PatList xs :: rest ->
       (aux [] xs) @ aux acc rest
    | _ :: rest -> aux acc rest
    | [] -> acc
  in
  aux [] [p]


let rec compile = function
  (* Identifier, constants, values *)
  | Name x ->  [IVar x]
  | ImpName x ->  [IImpVar x]
  | Tag x ->  [ITag x]
  | Int n -> [IInt n]
  | Bool b -> [IBool b]
  | String s -> [IString s]
  | List es -> [IStartL] @ compile_list es @ [IEndL]
  | Set es -> [IStartS] @ compile_list es @ [IEndS]
  | Tuple es -> [IStartT] @ compile_list es @ [IEndT]
  | Wildcard -> []
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
  | Let (x, e1, e2) -> (compile e1) @ [ILet x] @ (compile e2) @ [IUnscope (get_vars x)]
  | LetRec (x, e1, e2) ->
      [IThunk (force_thunks x (compile e1))] @
      [ILet (PatName x)] @ (force_thunks x (compile e2))
  | Assign (x, e) -> (compile e) @ [IAssign x]
  | Ref e -> (compile e) @ [IRef]
  | Deref e -> (compile e) @ [IDeref]
  | Match (e, es) ->
     let f acc = function
       | (p, expr) -> [IMatchCond (p, compile expr)] @ acc in
     [IStartM] @ (compile e) @ List.fold_left f [] (List.rev es) @ [IEndM]

  (* Conditionals *)
  | IfTE (e1, e2, e3) ->
      (compile e1) @ [IBranch (compile e2, compile e3)]
  | IfT (e1, e2) -> (compile e1) @ [ICond (compile e2)]
  | Req (e1, e2) -> (compile e1) @ [IReq] @ (compile e2)
  
  (* Lambda *)
  | Lam (x, e) ->
     let arg = 
       (match x with
       | Name x' -> x'
       | Unit -> "()"
       | _ -> error "invalid function parameter") in
     [IClosure ("anon", arg, compile e @ [IPopEnv])]
  | App (e1, e2) -> (compile e1) @ (compile e2) @ [ICall]
  
  (* Pi *)
  | Wr (e1, e2) -> (compile e1) @ (compile e2) @ [IWr (MHole, "")]
  | Rd x -> (compile x) @ [IRd ""]
  | Nu (xs, e) -> [INu xs] @ (compile e) @ [IUnscope [xs]]
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
  | Print e -> (compile e) @ [IPrint]
  | Rev e -> (compile e) @ [IRev]
and compile_list es = List.fold_left (fun acc e -> acc @ (compile e)) [] es
