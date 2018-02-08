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
        | (pid, ([], [v], _)) ->
            Printf.sprintf "%s%d:\n%s\n" "process" pid (Machine.string_of_mvalue v)
        | (pid, (frm :: _, _, _)) ->
            Printf.sprintf "%s%d:\n%s" "process" pid (Machine.string_of_frame frm)
        | (pid, ([], [], _)) ->
            Printf.sprintf "%s%d:\n0\n" "process" pid 
        | _ -> raise (Process_error "illegal final state")

    (* Run everything, including communication *)
    let run_full p = 
        let init_p = [Machine.run (0, ([p], [], [[]]))] in
        let rec loop = function
            | (true, ps') -> ps'
            | (false, ps') ->
                loop (Communication.run_comm (Machine.run_until_blocked ps'))
        in
            loop (Communication.run_comm (Machine.run_until_blocked init_p))

    let exec env = function
        | Syntax.Process p -> 
            (*print_endline (string_of_int (List.length p));*)
            (* Print ast *)
            (*print_endline (Syntax.string_of_expr p); env*)

            (* Print IR *)
            (*let instrs = Compile.compile p in
            print_endline (Machine.string_of_frame instrs); env*)

            (*let p = Compile.compile p in
            List.map print_endline (List.map string_of_finished_p (run_full p));*)
            env
end) ;;

ILC.main ()
