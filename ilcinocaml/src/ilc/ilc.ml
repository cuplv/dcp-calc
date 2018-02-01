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

    exception Process_error of string

    let string_of_finished_p = function
        | (pid, ([], [v], e)) -> Printf.sprintf "%s%d:\n%s\n" "process" pid (Print.string_of_mvalue v)
        | (pid, (frm :: frms, _, _)) -> Printf.sprintf "%s%d:\n%s" "process" pid (Print.string_of_frame frm)
        | (pid, ([] , [], e)) -> Printf.sprintf "%s%d:\n0\n" "process" pid 
        | _ -> raise (Process_error "illegal final state")

    (* Run everything, including communication *)
    let run_full ps = 
        let init_p = [Machine.run (0, ([ps], [], [[]]))] in
        let rec loop = function
            | (true, ps') -> ps'
            | (false, ps') -> loop (Communication.exec_comm (Machine.run_until_blocked ps'))
        in
            loop (Communication.exec_comm (Machine.run_until_blocked init_p))

    let exec env = function
        | Syntax.Process p ->
            (* Print ast *)
            (*print_endline (Print.string_of_expr p); env*)

            (* Print IR *)
            (*let instrs = Compile.compile p in
            print_endline (Print.string_of_frame instrs); env*)

            let ps = Compile.compile p in
            List.map print_endline (List.map string_of_finished_p (run_full ps)); env
end) ;;

ILC.main ()
