open Machine
open Printf

exception Compilation_error

(* Compiles expression into a list of abstract machine instructions *)
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
    | Syntax.Let (x, e1, e2) -> (compile e1) @ [ILet x] @ (compile e2)
    | Syntax.Lam (x, e) -> [IClosure ("anon", x, compile e @ [IPopEnv])]
    | Syntax.App (e1, e2) -> (compile e1) @ (compile e2) @ [ICall]
    | Syntax.ParComp (e1, e2) -> (compile e1) @ [IProc] @ (compile e2)
    | Syntax.Wr (e, x) -> (compile e) @ [IWr (MHole, x)]
    | Syntax.Rd (x1, x2) -> [IRd (x1, x2)]
    | Syntax.Seq (e1, e2) -> (compile e1) @ (compile e2)
    | _ -> raise (Compilation_error)

let rec string_of_frame = function
    | [] -> ""
    | i::is ->
        match i with
        | IVar x -> sprintf "IVar(%s)\n%s" x (string_of_frame is)
        | IInt n -> sprintf "IInt(%d)\n%s" n (string_of_frame is)
        | IBool b -> sprintf "IBool(%b)\n%s" b (string_of_frame is)
        | IAdd -> "IAdd\n" ^ string_of_frame is
        | ISub -> "ISub\n" ^ string_of_frame is
        | IMult -> "IMult\n" ^ string_of_frame is
        | IDiv -> "IDiv\n" ^ string_of_frame is
        | IMod -> "IMod\n" ^ string_of_frame is
        | ILess -> "ILess\n" ^ string_of_frame is
        | IClosure (_, x, f) ->
             sprintf "IClosure(%s\n%s)\n%s" x (string_of_frame f) (string_of_frame is)
        | IBranch (f1, f2) ->
             sprintf "IBranch(\n%s%s)" (string_of_frame f1) (string_of_frame f2)
        | ICall -> "ICall\n" ^ string_of_frame is
        | IPopEnv -> "IPopEnv\n" ^ string_of_frame is
        | ILet x -> sprintf "ILet(%s)\n%s" x (string_of_frame is)
        | IProc -> "IProc\n" ^ string_of_frame is
        | IWr (v, x) -> sprintf "IWr(%s,%s)\n%s" (string_of_mvalue v) x (string_of_frame is)
        | IRd (x1,x2) -> sprintf "IRd(%s,%s)\n%s" x1 x2 (string_of_frame is)
