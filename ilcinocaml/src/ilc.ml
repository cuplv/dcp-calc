let name = "ILC"

let read_more _ = false

let file_parser = Some (Parser.file Lexer.token)

let toplevel_parser = Some (Parser.toplevel Lexer.token)

exception Process_error of string

let printr = function
  | (_, ([], [v], _)) ->
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

let run_full env ps = 
  let init_ps = List.mapi (fun i p ->
                    Machine.run (i, ([p], [], [env]))) ps in
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

let exec verbose env = function
  | ps -> let f = function
            | Syntax.Process p -> Compile.compile p in
          let compiled_ps = List.map f ps in
          let print = if verbose then printr_verbose else printr in
          List.iter print (run_full env compiled_ps)

let load_prelude = function
  | ps -> let f = function
            | Syntax.Process p -> Compile.compile p in
          let compiled_ps = List.map f ps in
          (match (run_full [] compiled_ps) with
           | [(_, (_, _, env))] -> (List.hd env)
           | _ -> raise (Process_error "failed to load prelude"))
                                      
let interactive_shell = ref true

let wrapper = ref (Some ["rlwrap"; "ledit"])

let show_ast = ref false

let show_ir = ref false

let verbose = ref false

let prelude = ref ""

let file = ref None

let usage = 
  match file_parser with
  | Some _ -> "Usage: " ^ name ^ " [option] ... [file] ..."
  | None   -> "Usage:" ^ name ^ " [option] ..."


let add_file interactive filename = (file := Some (filename, interactive))

let options =
  Arg.align [
      ("-v",
       Arg.Unit (fun () ->
           print_endline (name ^ " " ^ "(" ^ Sys.os_type ^ ")");
           exit 0),
       " Print language information and exit");
      ("-n",
       Arg.Clear interactive_shell,
       " Do not run the interactive toplevel");
      ("-l",
       Arg.String (fun str -> add_file false str),
       "<file> Load <file> into the initial environment");
      ("--ast",
       Arg.Unit (fun () -> show_ast := true),
       " Print abstract syntax tree of source");
      ("--ir",
       Arg.Unit (fun () -> show_ir := true),
       " Print intermediate representation of source");
      ("--verbose",
       Arg.Unit (fun () -> verbose := true),
       " Print verbose execution output");
      ("--prelude",
       Arg.String (fun str -> prelude := str),
       " <file> Load <file> into prelude");    
    ]

let anonymous str =
  add_file true str;
  interactive_shell := false

(** Parse the contents from a file, using a given [parser]. *)
let read_file parser fn =
  try
    let fh = open_in fn in
    let lex = Lexing.from_channel fh in
    lex.Lexing.lex_curr_p <- {lex.Lexing.lex_curr_p with Lexing.pos_fname = fn};
    try
      let terms = parser lex in
      close_in fh;
      terms
    with
      (* Close the file in case of any parsing errors. *)
      Support.Error err -> close_in fh ; raise (Support.Error err)
  with
    (* Any errors when opening or closing a file are fatal. *)
    Sys_error msg -> Support.fatal_error "%s" msg

(** Parse input from toplevel, using the given [parser]. *)
let read_toplevel parser () =
  let prompt = ">>> "
  and prompt_more = String.make (String.length name) ' ' ^ "> " in
  print_string prompt ;
  let str = ref (read_line ()) in
  while read_more !str do
    print_string prompt_more ;
    str := !str ^ (read_line ()) ^ "\n"
  done ;
  parser (Lexing.from_string (!str ^ "\n"))

(** Parser wrapper that catches syntax-related errors and converts them to errors. *)
let wrap_syntax_errors parser lex =
  try
    parser lex
  with
  | _ ->
     Support.syntax_error ~loc:(Support.location_of_lex lex) "general confusion"

let get_prelude filename =
  match file_parser with
  | Some f ->
     let prelude_functions = read_file (wrap_syntax_errors f) filename in
     load_prelude prelude_functions
  | None -> Support.fatal_error "Cannot load prelude"        

(** Load directives from the given file. *)
let use_file (filename, interactive) =
  match file_parser with
  | Some f ->
     let processes = read_file (wrap_syntax_errors f) filename in
     (match (!show_ast, !show_ir) with
      | (true, _) -> print_ast processes
      | (_, true) -> print_ir processes
      | _ -> let prelude_env =  if !prelude <> "" then get_prelude !prelude
                                else [] in
             exec !verbose prelude_env processes)
  | None ->
     Support.fatal_error "Cannot load files, only interactive shell is available"
                               
(** Interactive toplevel *)
let toplevel ctx =
  (*let eof = match Sys.os_type with
      | "Unix" | "Cygwin" -> "Ctrl-D"
      | "Win32" -> "Ctrl-Z"
      | _ -> "EOF"
    in*)
  let toplevel_parser =
    match toplevel_parser with
    | Some p -> p
    | None -> Support.fatal_error "I am sorry but this language has no interactive toplevel."
  in
  Format.printf "%si, version 0.0.1 @." name ;
  try
    let _ = ref ctx in
    while true do
      try
        let prelude_env = if !prelude <> "" then get_prelude !prelude else [] in
        let cmd = read_toplevel (wrap_syntax_errors toplevel_parser) () in
        if !show_ast then print_ast cmd
        else if !show_ir then print_ir cmd
        else exec !verbose prelude_env cmd;
      with
      (*| Error err -> print_error err*)
      | Sys.Break -> prerr_endline "Interrupted."
    done
  with End_of_file -> ()

(** Main program *)
let main () =
  (* Intercept Ctrl-C by the user *)
  Sys.catch_break true;
  (* Parse the arguments. *)
  Arg.parse options anonymous usage;
  (* Attempt to wrap yourself with a line-editing wrapper. *)
  if !interactive_shell then
  (* Set the maximum depth of pretty-printing, after which it prints ellipsis. *)
  Format.set_max_boxes 42 ;
  Format.set_ellipsis_text "..." ;
  try
    match !file with
    | Some f -> use_file f
    | None -> if !interactive_shell then toplevel ()
  with
    Support.Error err -> Support.print_error err; exit 1

let () = main ()
