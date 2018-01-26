module ILC = Zoo.Main(struct
    let name = "ILC"
    
    type command = Syntax.process

    type environment = (string * int) list

    let options = []

    let initial_environment = []

    let read_more _ = false

    let file_parser = Some (Parser.file Lexer.token)

    let toplevel_parser = Some (Parser.toplevel Lexer.token)

    let exec env cmd = 
        match cmd with
            | Syntax.Process p -> print_endline (Syntax.string_of_expr p) ; env
end) ;;

ILC.main ()
