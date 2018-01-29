module ILC = Zoo.Main(struct
    let name = "ILC"
    
    type process = Syntax.process

    (* Typing context *)
    type ty_context = (Syntax.name * Syntax.ty) list

    type environment = (Syntax.name * Machine.mvalue) list

    let options = []

    let initial_environment = []

    let read_more _ = false

    let file_parser = Some (Parser.file Lexer.token)

    let toplevel_parser = Some (Parser.toplevel Lexer.token)

    let split instrs = 
        let add acc res = if acc<>[] then acc::res else res in
        let (res,acc) =
            List.fold_right (fun x (res,acc) ->
                match x with
                | Machine.IProc -> (add acc res, [])
                | _ -> (res, x::acc))
            instrs ([],[])
        in
        add acc res

    let string_of_process = function
        | (pid, ([], [v], e)) -> Printf.sprintf "%s%d:\n%s\n" "process" pid (Print.string_of_mvalue v)
        | (pid, (frm :: frms, _, _)) -> Printf.sprintf "%s%d:\n%s" "process" pid (Print.string_of_frame frm)
        | (pid, ([] , [], e)) -> Printf.sprintf "%s%d:\n0\n" "process" pid 

    let run ps = 
        let init_ps = List.mapi (fun i p -> Machine.run i ([p], [], [[]])) ps in
        let rec loop = function
            | (true, ps') -> ps'
            | (_, ps') -> loop (Communication.exec_comm (List.map
                (function | (pid, state) -> Machine.run pid state) ps'))
        in
            loop (Communication.exec_comm init_ps)

    let exec env = function
        | Syntax.Process p ->
            let ps = split (Compile.compile p) in
            List.map print_endline (List.map string_of_process (run ps)); env
end) ;;

ILC.main ()
