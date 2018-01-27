module ILC = Zoo.Main(struct
    let name = "ILC"
    
    type process = Syntax.process

    (* Typing context *)
    type ty_context = (Syntax.name * Syntax.ty) list

    (*type store = (Syntax.name * Machine.mvalue) list

    type environment = ty_context * store*)

    type environment = (Syntax.name * Machine.mvalue) list

    let options = []

    (*let initial_environment = ([], [])*)

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

    let print_state = function
        | (f::fs, s, e) -> Compile.string_of_frame f
    
    let exec env =  function
        | Syntax.Process p ->
            (*print_endline (Syntax.string_of_expr p);
            env*)

            (*let strings = List.map (Compile.string_of_frame) (split (Compile.compile p)) in
            List.iter print_endline strings;
            env*)
            let processes = split (Compile.compile p) in
            let rec run_this_shit = function
                | [] -> print_endline "done"
                | frm :: rest_frms -> 
                    let v = Machine.run frm env in
                    let string_of_val v = 
                        match v with
                        | Machine.MInt n -> string_of_int n ^ "\n"
                        | Machine.MBool b -> string_of_bool b ^ "\n"
                        | Machine.MClosure _ -> "lol\n"
                    in
                        print_endline (Compile.string_of_frame frm);
                        print_endline (print_state v); run_this_shit rest_frms
            in
            run_this_shit processes; env

            (*let frm = Compile.compile p in
            print_endline (Syntax.string_of_expr p);
            print_endline (Compile.string_of_frame frm);
            env*)
            
            (*let frm = Compile.compile p in
            let v = Machine.run frm env in
            let string_of_val v = 
                match v with
                | Machine.MInt n -> string_of_int n ^ "\n"
                | Machine.MBool b -> string_of_bool b ^ "\n"
                | Machine.MClosure _ -> "lol\n"
            in
                print_endline (Syntax.string_of_expr p);
                print_endline (Compile.string_of_frame frm);
                print_endline (string_of_val v);
                env*)
end) ;;

ILC.main ()
