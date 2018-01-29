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
    | IProc
    | IWr of mvalue * name
    | IRd of name * name
and frame = instr list
and environ = (name * mvalue) list
and stack = mvalue list

(* Convert machine value into string *)
let string_of_mvalue = function
    | MInt n -> string_of_int n
    | MBool b -> string_of_bool b
    | MClosure _ -> "<fun>"
    | MHole -> "hole"

let string_of_instr = function 
    | IVar x -> Printf.sprintf "IVar(%s)" x
    | IInt n -> Printf.sprintf "IInt(%d)" n
    | IBool b -> Printf.sprintf "IBool(%b)" b
    | IAdd -> "IAdd"
    | ISub -> "ISub"
    | IMult -> "IMult"
    | IDiv -> "IDiv"
    | IMod -> "IMod"
    | ILess -> "ILess"
    | IClosure (_, x, f) ->
         Printf.sprintf "IClosure(%s)" x 
    | IBranch (f1, f2) ->
         Printf.sprintf "IBranch()" 
    | ICall -> "ICall" 
    | IPopEnv -> "IPopEnv"
    | ILet x -> Printf.sprintf "ILet(%s)" x
    | IProc -> "IProc"
    | IWr (v, x) -> Printf.sprintf "IWr(%s,%s)" (string_of_mvalue v) x
    | IRd (x1, x2) -> Printf.sprintf "IRd(%s,%s)" x1 x2 

(*let string_of_environ = function
    | [] -> "\n"
    | (x, MInt i) :: rest -> Printf.sprintf "(%s,%d), " x i
    | (x, MBool b) :: rest -> Printf.sprintf "(%s,%b), " x b
    | (x, MClosure _) :: rest -> Printf.sprintf "(%s,Closure), " x*)

module ProcessSet = Set.Make(
    struct
        let compare = Pervasives.compare
        type t = frame * stack * environ
    end )

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

(* arithmetic *)
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

let exec instr frms stck envs = 
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
    | _ -> error ("executing " ^ string_of_instr instr)

let run pid state = 
    let rec loop = function
        | ([], [], e) -> (pid, ([], [], e))
        | ([], [v], e) -> (pid, ([], [v], e))
        | ((IRd (x1,x2) :: is) :: frms, stck, envs) -> (pid, ((IRd (x1,x2) :: is) :: frms, stck, envs))
        | ((IWr (MHole, x) :: is) :: frms, v :: stck, envs) -> (pid, ((IWr (v,x) :: is) :: frms, stck, envs))
        | ((IWr (v,x) :: is) :: frms, stck, envs) -> (pid, ((IWr (v,x) :: is) :: frms, stck, envs))
        | ((i::is) :: frms, stck, envs) -> loop (exec i (is::frms) stck envs)
        | ([] :: frms, stck, envs) -> loop (frms, stck, envs)
        | _ -> error "illegal end of program"
    in
        loop state
