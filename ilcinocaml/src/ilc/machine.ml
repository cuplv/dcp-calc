type name = Syntax.name

type mvalue =
    | MInt of int
    | MBool of bool
    | MClosure of name * frame * environ
    | MHole
and instr =
    | IVar of name
    | IInt of int
    | IBool of bool
    | IAdd
    | ISub
    | IMult
    | IDiv
    | IMod
    | ILess
    | IClosure of name * name * frame
    | IBranch of frame * frame
    | ICall
    | IPopEnv
    | ILet of name
    | IStartP of int
    | IEndP of int
    | IWr of mvalue * name
    | IRd of name * name
    | ISpawn
    | IHole of int
and frame = instr list
and environ = (name * mvalue) list
and stack = mvalue list

open Printf

(* Convert machine value into string *)
let string_of_mvalue = function
    | MInt n -> string_of_int n
    | MBool b -> string_of_bool b
    | MClosure _ -> "<fun>"
    | MHole -> "hole"

(* Convert instruction into string *)
let string_of_instr = function 
    | IVar x -> sprintf "IVar(%s)" x
    | IInt n -> sprintf "IInt(%d)" n
    | IBool b -> sprintf "IBool(%b)" b
    | IAdd -> "IAdd"
    | ISub -> "ISub"
    | IMult -> "IMult"
    | IDiv -> "IDiv"
    | IMod -> "IMod"
    | ILess -> "ILess"
    | IClosure (_, x, f) ->
         sprintf "IClosure(%s)" x 
    | IBranch (f1, f2) ->
         sprintf "IBranch()" 
    | ICall -> "ICall" 
    | IPopEnv -> "IPopEnv"
    | ILet x -> sprintf "ILet(%s)" x
    | IWr (v, x) -> sprintf "IWr(%s,%s)" (string_of_mvalue v) x
    | IRd (x1, x2) -> sprintf "IRd(%s,%s)" x1 x2 
    | IStartP n -> sprintf "IStartP(%d)" n
    | IEndP n -> sprintf "IEndP(%d)" n
    | ISpawn -> "ISpawn"
    | IHole n -> sprintf "IHole(%d)" n

(* Convert instruction list into string *)
let rec string_of_frame = function
    | [] -> ""
    | i::is -> string_of_instr i ^ string_of_frame is

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
    | (MInt x) :: (MInt y) :: s -> let res = MInt (y / x) :: s in 
                                   if x <> 0 then res else error "division by 0"
    | _ -> error "int and int expected in div"

let modu = function
    | (MInt x) :: (MInt y) :: s -> MInt (y mod x) :: s
    | _ -> error "int and int expected in mod"

let less = function
    | (MInt x) :: (MInt y) :: s -> MBool (y < x) :: s
    | _ -> error "int and int expected in less"

let split frm n = 
    let rec aux acc = function
        | [] -> (List.rev acc, [])
        | IEndP n' :: rest when n=n' -> (List.rev acc, rest)
        | i::is -> aux (i::acc) is
    in
    aux [] frm

let get_par_processes frm n =
    let (first_par, rest) = split frm n in
    let (second_par, rest) = split (List.tl rest) (n+1) in
    (first_par, second_par, rest)

let remove_last l = List.rev (List.tl (List.rev l))

let exec pid instr frms stck envs = 
    match instr with
    | IAdd -> (frms, add stck, envs)
    | ISub -> (frms, sub stck, envs)
    | IMult -> (frms, mult stck, envs)
    | IDiv -> (frms, div stck, envs)
    | IMod -> (frms, modu stck, envs)
    | ILess -> (frms, less stck, envs)
    | IVar x -> (frms, (lookup x envs) :: stck, envs)
    | IInt n -> (frms, (MInt n) :: stck, envs)
    | IBool b -> (frms, (MBool b) :: stck, envs)
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
            let updated_env = (x, x') :: env in
            (frms, stck', updated_env :: env_tail)
        | [] -> error "no environment for variable")
    | IBranch (f1, f2) ->
        let (b, stck') = pop_bool stck in
        ((if b then f1 else f2) :: frms, stck', envs)
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
            let (first_par, second_par, rest) = get_par_processes frm n in
            ([ISpawn] :: first_par :: second_par :: rest :: rest_frms, stck, envs)
        | [] -> error "no instrs to spawn")
    | _ -> error (string_of_instr instr)

let run pid state = 
    let rec loop = function
        | ([], [], e) -> (pid, ([], [], e))
        | ([], [v], e) -> (pid, ([], [v], e))
        | ((IRd (x1, x2) :: is) :: frms, stck, envs) ->
            (pid, ((IRd (x1, x2) :: is) :: frms, stck, envs))
        | ((IWr (MHole, x) :: is) :: frms, v :: stck, envs) ->
            (pid, ((IWr (v, x) :: is) :: frms, stck, envs))
        | ((IWr (v, x) :: is) :: frms, stck, envs) ->
            (pid, ((IWr (v, x) :: is) :: frms, stck, envs))
        | ([ISpawn] :: frms, stck, envs) -> (pid, ([ISpawn] :: frms, stck, envs))
        | ((IHole n :: is) :: frms, stck, envs) -> (pid, ((IHole n :: is) :: frms, stck, envs))
        | ((i :: is) :: frms, stck, envs) ->
            loop (exec pid i (is :: frms) stck envs)
        | ([] :: frms, stck, envs) -> loop (frms, stck, envs)
        | s -> error ("illegal end of program" ^ (string_of_int pid) ^ (string_of_state s))
    in
        loop state
