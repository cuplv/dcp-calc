open Machine

exception Compilation_error

(* Compiles expression into frame, i.e., list of instructions *)
let rec compile = function
    | Syntax.Name x ->  [IVar x]
    | Syntax.Int n -> [IInt n]
    | Syntax.Bool b -> [IBool b]
    | Syntax.Plus (e1, e2) -> (compile e1) @ (compile e2) @ [IAdd]
    | Syntax.Minus (e1, e2) -> (compile e1) @ (compile e2) @ [ISub]
    | Syntax.Times (e1, e2) -> (compile e1) @ (compile e2) @ [IMult]
    | Syntax.Divide (e1, e2) -> (compile e1) @ (compile e2) @ [IDiv]
    | Syntax.Mod (e1, e2) -> (compile e1) @ (compile e2) @ [IMod]
    | Syntax.Less (e1, e2) -> (compile e1) @ (compile e2) @ [ILess]
    | Syntax.If (e1, e2, e3) -> (compile e1) @ [IBranch (compile e2, compile e3)]
    (*| Syntax.Let*)
    | Syntax.Lam (x, e) -> [IClosure ("lol", x, compile e @ [IPopEnv])]
    | Syntax.App (e1, e2) -> (compile e1) @ (compile e2) @ [ICall]
    | _ -> raise (Compilation_error)

let rec string_of_frame = function
    | [] -> ""
    | i::is -> match i with
               | IVar x -> "IVar(" ^ x ^ ")\n" ^ string_of_frame is
               | IInt n -> "IInt(" ^ string_of_int n ^ ")\n" ^ string_of_frame is
               | IBool b -> "IBool(" ^ string_of_bool b ^ ")\n" ^ string_of_frame is
               | IAdd -> "IAdd\n" ^ string_of_frame is
               | ISub -> "ISub\n" ^ string_of_frame is
               | IMult -> "IMult\n" ^ string_of_frame is
               | IDiv -> "IDiv\n" ^ string_of_frame is
               | IMod -> "IMod\n" ^ string_of_frame is
               | ILess -> "ILess\n" ^ string_of_frame is
               | IClosure (_, x, f) -> "IClosure(" ^ x ^ "\n" ^
                                    string_of_frame f ^ ")\n" ^ string_of_frame is
               | IBranch (f1, f2) -> "IBranch(\n" ^ string_of_frame f1 ^ 
                                     string_of_frame f2 ^ ")" ^ string_of_frame is
               | ICall -> "ICall\n" ^ string_of_frame is
               | IPopEnv -> "IPopEnv\n" ^ string_of_frame is
