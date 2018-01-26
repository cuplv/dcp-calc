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

    let exec env =  function
        | Syntax.Process p ->

            (*print_endline (Syntax.string_of_expr p);
            env*)

            (*let frm = Compile.compile p in
            print_endline (Syntax.string_of_expr p);
            print_endline (Compile.string_of_frame frm);
            env*)
            
            let frm = Compile.compile p in
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
                env
end) ;;

ILC.main ()
