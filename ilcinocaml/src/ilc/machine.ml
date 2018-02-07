(* -------------------------------------------------------------------------- *)
(* Abstract machine *)

open Printf

type name = Syntax.name

type mvalue =
    | MInt of int
    | MBool of bool
    | MString of string
    | MList of mvalue list
    | MTuple of mvalue list
    | MClosure of name * frame * environ
    | MThunk of frame
    | MHole
    | MVarP of name
and instr =
    | IVarP of name
    | IVar of name
    | IInt of int
    | IBool of bool
    | IString of string
    | IAdd
    | ISub
    | IMult
    | IDiv
    | IMod
    | ILt
    | IGt
    | ILeq
    | IGeq
    | IOr
    | IAnd
    | INot
    | IEq
    | INeq
    | IClosure of name * name * frame
    | IBranch of frame * frame
    | ICond of frame
    | ICall
    | INu of name list
    | IPopEnv
    | IThunk of frame
    | IForce
    | ILet of name
    | ILetP
    | IStartP of int
    | IEndP of int
    | IChoice of int * int * instr
    | IBlock of instr
    | IWr of mvalue * name
    | IRdBind of name * name
    | IRd of name
    | ISpawn
    | IHole of int
    | IStartL
    | IEndL
    | ICons
    | IConcat
    | IStartT
    | IEndT
    | IFst
    | ISnd
    | IRepl of frame
    | IRand
    | IShow
    | ILookup
and frame = instr list
and environ = (name * mvalue) list
and stack = mvalue list

(* -------------------------------------------------------------------------- *)
(* Printing *)

let string_of_list f l = 
    let rec to_str acc = function
        | [] -> acc
        | [v] -> acc ^ f v
        | v :: vs -> to_str (acc ^ f v ^ ",") vs
    in
    to_str "" l

let rec string_of_mvalue = function
    | MInt n -> string_of_int n
    | MBool b -> string_of_bool b
    | MString s -> s
    | MThunk _ -> "<thunk>"
    | MClosure _ -> "<fun>"
    | MHole -> "hole"
    | MList l -> "[" ^ string_of_list string_of_mvalue l ^ "]"
    | MTuple l -> "(" ^ string_of_list string_of_mvalue l ^ ")"
    | MVarP p -> p

let rec string_of_instr = function 
    | IVar x -> sprintf "IVar(%s)" x
    | IVarP x -> sprintf "IVarP(%s)" x
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
    | IBranch _ -> "IBranch"
    | ICond _ -> "ICond"
    | ICall -> "ICall" 
    | INu _ -> "INu" (* TODO: Print *)
    | IPopEnv -> "IPopEnv"
    | IThunk e -> "IThunk" ^ List.fold_left (fun acc x -> acc ^ "," ^ string_of_instr x) "" e
    | IForce -> "IForce"
    | ILet x -> sprintf "ILet(%s)" x
    | ILetP -> "ILetP"
    | IWr (v, x) -> sprintf "IWr(%s,%s)" (string_of_mvalue v) x
    | IRdBind (x1, x2) -> sprintf "IRdBind(%s,%s)" x1 x2 
    | IRd x -> sprintf "IRd(%s)" x
    | IStartP n -> sprintf "IStartP(%d)" n
    | IEndP n -> sprintf "IEndP(%d)" n
    | IChoice (p, c, i) -> sprintf "IChoice(%d,%d,%s)" p c (string_of_instr i)
    | IBlock i -> sprintf "IBlock(%s)" (string_of_instr i)
    | ISpawn -> "ISpawn"
    | IHole n -> sprintf "IHole(%d)" n
    | IStartL -> "IStartL"
    | IEndL -> "IEndL"
    | ICons -> "ICons"
    | IConcat -> "IConcat"
    | IStartT -> "IStartT"
    | IEndT -> "IEndT"
    | IFst -> "IFst"
    | ISnd -> "ISnd"
    | IRepl _ -> "IRepl"
    | IRand -> "IRand"
    | IShow -> "IShow"
    | ILookup -> "ILookup"

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

let string_of_frames frms =
    let rec to_str = function
        | [] -> ""
        | f :: fs -> string_of_frame f ^ to_str fs
    in
        "[" ^ to_str frms ^ "]"

let string_of_environs envs =
    let rec to_str = function
        | [] -> ""
        | e :: es -> string_of_environ e ^ to_str es
    in
        "[" ^ to_str envs ^ "]"

let string_of_state = function
    | (f, s, e) -> string_of_frames f ^ string_of_stack s ^ string_of_environs e

let string_of_process = function
    | (pid, s) -> Printf.sprintf "Process: %d\n %s" pid (string_of_state s)

(* -------------------------------------------------------------------------- *)
(* Abstract machine *)

exception Machine_error of string

let error msg = raise (Machine_error msg)

let lookup x = function
    | env::_ -> (try List.assoc x env with Not_found -> error ("unknown " ^ x))
    | _ -> error "no environment to look up"

let pop = function
    | [] -> error "empty stack"
    | v::s -> (v, s)

let pop_bool = function
    | MBool b :: s -> (b, s)
    | _ -> error "bool expected"

let pop_app = function
    | v :: MClosure (x, f, e) :: s -> (x, f, e, v, s)
    | _ -> error "value and closure expected"

let pop_list l = 
    let rec pop acc = function
        | MList [] :: s -> (acc, s)
        | mv :: s -> pop (mv :: acc) s
        | _ -> error "no list to pop"
    in
    pop [] l

let add = function
    | (MInt x) :: (MInt y) :: s -> MInt (y + x) :: s
    | _ -> error "int and int expected in add"

let sub = function
    | (MInt x) :: (MInt y) :: s -> MInt (y - x) :: s
    | _ -> error "int and int expected in sub"

let mult = function
    | (MInt x) :: (MInt y) :: s -> MInt (y * x) :: s
    | _ -> error "int and int expected in mult"

let div = function
    | (MInt x) :: (MInt y) :: s ->
        let res = MInt (y / x) :: s in 
        if x <> 0 then res else error "division by 0"
    | _ -> error "int and int expected in div"

let modu = function
    | (MInt x) :: (MInt y) :: s -> MInt (y mod x) :: s
    | _ -> error "int and int expected in mod"

let lt = function
    | (MInt x) :: (MInt y) :: s -> MBool (y < x) :: s
    | _ -> error "int and int expected in lt"

let gt = function
    | (MInt x) :: (MInt y) :: s -> MBool (y > x) :: s
    | _ -> error "int and int expected in gt"

let leq = function
    | (MInt x) :: (MInt y) :: s -> MBool (y <= x) :: s
    | _ -> error "int and int expected in leq"

let geq = function
    | (MInt x) :: (MInt y) :: s -> MBool (y >= x) :: s
    | _ -> error "int and int expected in geq"

let l_or = function
    | (MBool x) :: (MBool y) :: s -> MBool (x || y) :: s
    | _ -> error "int and int expected in or"

let l_and = function
    | (MBool x) :: (MBool y) :: s -> MBool (x && y) :: s
    | _ -> error "int and int expected in and"

let l_not = function
    | (MBool x) :: s -> MBool (not x) :: s
    | _ -> error "int and int expected in not"

let eq = function
    | (MInt x) :: (MInt y) :: s -> MBool (x = y) :: s
    | (MString x) :: (MString y) :: s -> MBool (x = y) :: s
    | _ -> error "invalid operands in equal"

let neq = function
    | (MInt x) :: (MInt y) :: s -> MBool (x <> y) :: s
    | (MString x) :: (MString y) :: s -> MBool (x <> y) :: s
    | _ -> error "invalid operands in neq"

let cons = function
    | (MList x) :: y :: s -> MList (y::x) :: s
    | _ -> error "no list to cons"

let concat = function
    | (MString x) :: (MString y) :: s -> MString (y ^ x) :: s
    | _ -> error "no strings to concat"

let do_fst = function
    | MTuple [x;y] :: s -> x :: s
    | _ -> error "no pair to fst"

let do_snd = function
    | MTuple [x;y] :: s -> y :: s
    | _ -> error "no pair to snd"

let rand = function
    | s -> MInt (Random.bits ()) :: s

let show = function
    | (MInt x) :: s -> MString (string_of_int x) :: s
    | _ -> error "no int to show"

let assoc_lookup = function
    | (MList xs) :: x :: s ->
        let rec find = function
            | [] -> error "not found in assoc list"
            | MTuple [k;v] :: ts when x=k -> v
            | _ :: ts -> find ts
        in
        (find xs) :: s
    | _ -> error "no assoc list"

let split frm n = 
    let rec aux acc = function
        | [] -> (List.rev acc, [])
        | IEndP n' :: rest when n=n' -> (List.rev acc, rest)
        | i::is -> aux (i::acc) is
    in
    aux [] frm

let get_par_ps frm n =
    let (fst_frm, rest_frm) = split frm n in
    let (snd_frm, rest_frm) = split (List.tl rest_frm) (succ n) in
    (fst_frm, snd_frm, rest_frm)

let exec instr frms stck envs = 
    match instr with
    | IAdd -> (frms, add stck, envs)
    | ISub -> (frms, sub stck, envs)
    | IMult -> (frms, mult stck, envs)
    | IDiv -> (frms, div stck, envs)
    | IMod -> (frms, modu stck, envs)
    | ILt -> (frms, lt stck, envs)
    | IGt -> (frms, gt stck, envs)
    | ILeq -> (frms, leq stck, envs)
    | IGeq -> (frms, geq stck, envs)
    | IOr -> (frms, l_or stck, envs)
    | IAnd -> (frms, l_and stck, envs)
    | INot -> (frms, l_not stck, envs)
    | IEq -> (frms, eq stck, envs)
    | INeq -> (frms, neq stck, envs)
    | IVar x -> (frms, (lookup x envs) :: stck, envs)
    | IVarP x -> (frms, (MVarP x) :: stck, envs)
    | IInt n -> (frms, (MInt n) :: stck, envs)
    | IBool b -> (frms, (MBool b) :: stck, envs)
    | IString s -> (frms, (MString s) :: stck, envs)
    | IThunk f -> (frms, (MThunk f) :: stck, envs)
    | IForce ->
        (match frms with
        | frm :: frm_rest ->
            (match pop stck with
            | (MThunk f, stck') ->
                (f :: frm :: frm_rest, stck', envs)
            | _ -> error "no thunk to pop")
        | _ -> error "no frames")
    | IClosure (f, x, frm) ->
        (match envs with
        | env :: _ ->
            let named =
                match frms with
                | (ILet x :: _) :: _ -> x
                | _ -> f
            in
            let rec c = MClosure (x, frm, (named, c) :: env)
            in (frms, c :: stck, envs)
        | [] -> error "no environment for a closure")
    | ILet x ->
        (match envs with
        | env :: env_tail ->
            let (x', stck') = pop stck in
            let new_mapping = (x, x') :: env in
            (frms, stck', new_mapping :: env_tail)
        | [] -> error "no environment for variable")
    | ILetP -> (* TODO: Can probably do this in a better way *)
        (match envs with
        | env :: env_tail ->
            let (pattern, stck') = pop stck in
            let (tuple, stck') = pop stck' in
            let values =
                (match tuple with
                | MTuple vs -> vs
                | _ -> error "pattern match failed") in
            let keys = 
                (match pattern with
                | MList ks -> ks
                | _ -> error "pattern match failed") in
            let new_mappings =
                (try List.combine keys values with
                | Invalid_argument _ ->
                    error "pattern match failed") in
            let new_mappings = 
                List.fold_left (fun acc x ->
                    match x with 
                    | (MVarP x, y) -> (x, y) :: acc
                    | (MString x, MString y) when x=y -> acc
                    | (MInt x, MInt y) when x=y -> acc
                    | _ -> error "pattern match failed")
                [] (List.rev new_mappings) in
            let updated_env = new_mappings @ env in
            (frms, stck', updated_env :: env_tail)
        | [] -> error "no environment for variable")
    | INu xs ->
        (match envs with
        | env :: env_tail ->
            let new_mapping =
                List.fold_left (fun acc x -> (x, MHole) :: acc) [] (List.rev xs) in
            (frms, stck, (new_mapping @ env) :: env_tail)
        | [] -> error "no environment for variable")
    | IBranch (f1, f2) ->
        let (b, stck') = pop_bool stck in
        ((if b then f1 else f2) :: frms, stck', envs)
    | ICond f ->
        let (b, stck') = pop_bool stck in
        ((if b then f else []) :: frms, stck', envs)
    | ICall ->
        let (x, frm, env, v, stck') = pop_app stck in
        (frm :: frms, stck', ((x,v) :: env) :: envs)
    | IPopEnv ->
        (match envs with
        | [] -> error "no environment to pop"
        | _ :: envs' -> (frms, stck, envs'))
    | IStartP n ->
        (match frms with
        | frm :: rest_frms ->
            let (fst_frm, snd_frm, rest_frm) = get_par_ps frm n in
            ([ISpawn] :: fst_frm :: snd_frm :: rest_frm :: rest_frms, stck, envs)
        | [] -> error "no processes to spawn")
    | IStartL -> (frms, (MList []) :: stck, envs)
    | IEndL ->
        let (lst, stck') = pop_list stck
        in (frms, (MList lst) :: stck', envs)
    | ICons -> (frms, cons stck, envs)
    | IConcat -> (frms, concat stck, envs)
    | IStartT -> (frms, (MList []) :: stck, envs)
    | IEndT ->
        let (lst, stck') = pop_list stck
        in (frms, (MTuple lst) :: stck', envs)
    | IFst -> (frms, do_fst stck, envs)
    | ISnd -> (frms, do_snd stck, envs)
    | IRand -> (frms, rand stck, envs)
    | IShow -> (frms, show stck, envs)
    | ILookup -> (frms, assoc_lookup stck, envs)
    | _ -> error ("illegal instruction")

(* Execute instructions *)
(* TODO: Generalize read instructions *)
let run p = 
    let rec loop = function
        | (pid, ([], [], e)) -> (pid, ([], [], e))
        | (pid, ([], [v], e)) -> (pid, ([], [v], e))
        | (pid, ((IRdBind (x1, x2) :: is) :: frms, stck, envs)) ->
            if List.mem_assoc x2 (List.hd envs)
            then (pid, ((IRdBind (x1, x2) :: is) :: frms, stck, envs))
            else error "channel not allocated"
        | (pid, ((IChoice(pid', cid,  (IRdBind (x1, x2))) :: is) :: frms, stck, envs)) ->
            if List.mem_assoc x2 (List.hd envs)
            then (pid, ((IChoice(pid', cid, (IRdBind (x1, x2))) :: is) :: frms, stck, envs))
            else error "channel not allocated"
        | (pid, ((IRd x :: is) :: frms, stck, envs)) ->
            if List.mem_assoc x (List.hd envs)
            then (pid, ((IRd x :: is) :: frms, stck, envs))
            else error "channel not allocated"
        | (pid, ((IChoice(pid', cid,  (IRd x)) :: is) :: frms, stck, envs)) ->
            if List.mem_assoc x (List.hd envs)
            then (pid, ((IChoice(pid', cid, (IRd x)) :: is) :: frms, stck, envs))
            else error "channel not allocated"
        | (pid, ((IWr (MHole, x) :: is) :: frms, v :: stck, envs)) ->
            if List.mem_assoc x (List.hd envs)
            then (pid, ((IWr (v, x) :: is) :: frms, stck, envs))
            else error "channel not allocated"
        | (pid, ((IWr (v, x) :: is) :: frms, stck, envs)) ->
            if List.mem_assoc x (List.hd envs)
            then (pid, ((IWr (v, x) :: is) :: frms, stck, envs))
            else error "channel not allocated"
        | (pid, ([ISpawn] :: frms, stck, envs)) ->
            (pid, ([ISpawn] :: frms, stck, envs))
        | (pid, ((IHole n :: is) :: frms, stck, envs)) ->
            (pid, ((IHole n :: is) :: frms, stck, envs))
        | (pid, ((IBlock i :: is) :: frms, stck, envs)) ->
            (pid, ((IBlock i :: is) :: frms, stck, envs))
        | (pid, ((IRepl i :: is) :: frms, stck, envs)) ->
            (pid, ((IRepl i :: is) :: frms, stck, envs))
        | (pid, ((i :: is) :: frms, stck, envs)) ->
            loop (pid, (exec i (is :: frms) stck envs))
        | (pid, ([] :: frms, stck, envs)) -> loop (pid, (frms, stck, envs))
        | s -> error ("illegal end of program")
    in
        loop p

let run_all ps = List.map run ps

(* Spawns new processes *)
let pid_counter = ref 1

let spawn_all ps = 
    let rec spawn old_ps new_ps = function
        | [] -> if List.length new_ps = 0
                then (true, List.rev old_ps)
                else (false, (List.rev old_ps) @ (List.rev new_ps))
        | (pid, ([ISpawn] :: frm1 :: frm2 :: frms, stck, envs)) :: rest_ps ->
            let pid' = !pid_counter in
            let original_p = (pid, (frms, stck, envs)) in
            let new_p1 = (pid', ([frm1], [], envs)) in
            let new_p2 = (succ pid', ([frm2], [], envs)) in
            pid_counter := !pid_counter + 2;
            spawn (original_p :: old_ps)
                  (new_p2 :: new_p1 :: new_ps)
                  rest_ps
        | p :: rest_ps -> spawn (p :: old_ps) new_ps rest_ps
    in
        spawn [] [] ps

(* Fill holes *)
let is_hole = function
    | (_, ((IHole _ :: _) :: _, _, _)) -> true
    | _ -> false

let fill_hole ps = function
    | (pid, ((IHole n :: instrs) :: frms, stck, envs)) ->
        let rec check_ps = function
            | (pid', ([], [v], _)) :: _ when n=pid' ->
                (false, (pid, (instrs :: frms, v :: stck, envs)))
            | (pid', _) :: _ when n=pid' ->
                (true, (pid, ((IHole n :: instrs) :: frms, stck, envs)))
            | _ :: rest_ps -> check_ps rest_ps
            | [] -> error ("process not found")
        in
        check_ps ps
    | _ -> error ("not a hole")

let fill_all ps =
    let rec loop acc is_done = function
        | [] -> (is_done, List.rev acc)
        | p :: ps' ->
            if (is_hole p)
            then let (is_done', new_p) = fill_hole ps p in
                 loop (new_p :: acc) is_done' ps'
            else loop (p :: acc) is_done ps'
    in loop [] true ps

(* Executes instructions, spawns processes, and fills holes until blocked *)
let run_until_blocked ps =  
    let quit_loop = ref false in
    let prev_done_spawning = ref false in
    let prev_done_filling = ref false in
    let ps_store = ref ps in
    while not !quit_loop do
        let (done_spawning, ps') = spawn_all (run_all !ps_store) in
        let (done_filling, ps') = fill_all (run_all ps') in
        quit_loop := done_spawning && !prev_done_spawning &&
                     done_filling && !prev_done_filling;
        prev_done_spawning := done_spawning;
        prev_done_filling := done_filling;
        ps_store := ps';
    done; !ps_store
