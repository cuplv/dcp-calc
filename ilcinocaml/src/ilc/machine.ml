type name = Syntax.name

type mvalue =
    | MInt of int
    | MBool of bool
    | MClosure of name * frame * environ
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
    | IWr of name
    | IRd of name
and frame = instr list
and environ = (name * mvalue) list
and stack = mvalue list

(*let string_of_instr = function 
    | IVar x -> Printf.sprintf "IVar(%s)\n%s" x (string_of_frame is)
    | IInt n -> Printf.sprintf "IInt(%d)\n%s" n (string_of_frame is)
    | IBool b -> Printf.sprintf "IBool(%b)\n%s" b (string_of_frame is)
    | IAdd -> "IAdd\n" ^ string_of_frame is
    | ISub -> "ISub\n" ^ string_of_frame is
    | IMult -> "IMult\n" ^ string_of_frame is
    | IDiv -> "IDiv\n" ^ string_of_frame is
    | IMod -> "IMod\n" ^ string_of_frame is
    | ILess -> "ILess\n" ^ string_of_frame is
    | IClosure (_, x, f) ->
         Printf.sprintf "IClosure(%s\n%s)\n%s" x (string_of_frame f) (string_of_frame is)
    | IBranch (f1, f2) ->
         Printf.sprintf "IBranch(\n%s%s)" (string_of_frame f1) (string_of_frame f2)
    | ICall -> "ICall\n" ^ string_of_frame is
    | IPopEnv -> "IPopEnv\n" ^ string_of_frame is
    | ILet x -> Printf.sprintf "ILet(%s)\n%s" x (string_of_frame is)
    | IProc -> "IProc\n" ^ string_of_frame is
    | IWr x -> Printf.sprintf "IWr(%s)\n%s" x (string_of_frame is)
    | IRd x -> Printf.sprintf "IRd(%s)\n%s" x (string_of_frame is)

let string_of_environ = function
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

(* Convert machine value into string *)
let string_of_mvalue = function
    | MInt n -> string_of_int n
    | MBool b -> string_of_bool b
    | MClosure _ -> "<fun>"

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
    (* Arithmetic *)
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
        match envs with
        | [] -> error "no environment to pop"
        | _ :: envs' -> (frms, stck, envs')
    (*| IWr _ -> (frms, stck, envs)
    | IRd _ -> (frms, stck, envs)*)

(*let wtf = ProcessSet.singleton(([], [], []))*)

let run frm env = 
    let rec loop = function
        (*| ([], [v], _) -> v*)
        | ([], [v], e) -> ([], [v], e)
        | ((IRd x ::is) :: frms, stck, envs) -> ((IRd x ::is) :: frms, stck, envs)
        | ((IWr x ::is) :: frms, stck, envs) -> ((IWr x ::is) :: frms, stck, envs)
        | ((i::is) :: frms, stck, envs) -> loop (exec i (is::frms) stck envs)
        | ([] :: frms, stck, envs) -> loop (frms, stck, envs)
        | _ -> error "illegal end of program"
    in
        loop ([frm], [], [env])
