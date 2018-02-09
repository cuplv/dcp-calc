module ILC = Zoo.Main(struct
    let name = "ILC"
    
    type process = Syntax.process

    type ty_context = (Syntax.name * Syntax.ty) list

    type environment = (Syntax.name * Machine.mvalue) list

    let options = []

    let initial_environment = []

    let read_more _ = false

    let file_parser = Some (Parser.file Lexer.token)

    let toplevel_parser = Some (Parser.toplevel Lexer.token)

    exception Process_error of string

    let printr = function
        | (_, ([], [v], _)) when v <> Machine.MInt 0 ->
            Printf.printf "%s\n" (Machine.string_of_mvalue v)
        | _ -> ()

    let printr_verbose = function
        | (pid, ([], [v], _)) ->
            Printf.printf "%s%d:\n%s\n" "process" pid (Machine.string_of_mvalue v)
        | (pid, (frm :: _, _, _)) ->
            Printf.printf "%s%d:\n%s" "process" pid (Machine.string_of_frame frm)
        | (pid, ([], [], _)) ->
            Printf.printf "%s%d:\n0\n" "process" pid 
        | _ -> raise (Process_error "illegal final state")

    (* Run everything, including communication *)
    let run_full ps = 
        let init_ps = List.mapi (fun i p ->
            Machine.run (i, ([p], [], [[]]))) ps in
        let rec loop = function
            | (true, ps') -> ps'
            | (false, ps') ->
                loop (Communication.run_comm (Machine.run_until_blocked ps'))
        in
            Machine.init_pid_counter (List.length init_ps);
            loop (Communication.run_comm (Machine.run_until_blocked init_ps))

    let print_ast = function
        | ps -> let f acc x =
                    (match x with
                    | Syntax.Process p ->
                        acc ^ Syntax.string_of_expr p ^ "\n") in
                print_string (List.fold_left f "" ps)

    let print_ir = function
        | ps -> let f acc x =
                    (match x with
                    | Syntax.Process p ->
                        acc ^ (Machine.string_of_frame (Compile.compile p)) ^ "\n") in
                print_string (List.fold_left f "" ps)

    let exec verbose = function
        | ps -> let f = function
                    | Syntax.Process p -> Compile.compile p in
                let compiled_ps = List.map f ps in
                let print = if verbose then printr_verbose else printr in
                List.iter print (run_full compiled_ps)
end) ;;

ILC.main ()
