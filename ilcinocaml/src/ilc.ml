let name = "ILC"

let version = "0.0.1"

let interactive_shell = ref true

let show_ast = ref false

let show_ir = ref false

let verbose = ref false

let prelude_path = ref None

let file = ref None         

let read_more _ = false

let file_parser = Parser.file Lexer.token

let toplevel_parser = Parser.toplevel Lexer.token

let options =
  Arg.align [
      ("--version",
       Arg.Unit (fun () ->
           print_endline ("The Interactive Lambda Calculus, \
                           version " ^ version);
           exit 0),
       " Print version number");
      ("--ast",
       Arg.Unit (fun () -> show_ast := true),
       " Print abstract syntax tree");
      ("--ir",
       Arg.Unit (fun () -> show_ir := true),
       " Print intermediate representation");
      ("--verbose",
       Arg.Unit (fun () -> verbose := true),
       " Print verbose execution output");
      ("--prelude",
       Arg.String (fun path -> prelude_path := Some path),
       "<file> Load <file> into prelude");
    ]

let parse_args () =
  Arg.parse options
            (fun filename -> file := Some filename;
                             interactive_shell := false)
            ("Usage: " ^ name ^ " [options] ... [file] ...")

let parse_file parser filename =
  try
    let fh = open_in filename in
    let lex = Lexing.from_channel fh in
    lex.Lexing.lex_curr_p <- {lex.Lexing.lex_curr_p with Lexing.pos_fname = filename};
    try
      let results = parser lex in
      close_in fh;
      results
    with
    | Support.Error err -> close_in fh ; raise (Support.Error err)
    | _ -> Support.syntax_error
             ~loc:(Support.location_of_lex lex) "general confusion"

  with
    Sys_error msg -> Support.fatal_error "%s" msg

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

exception Runtime_error of string

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
  | _ -> raise (Runtime_error "illegal final state")

let print_ast = function
  | ps ->
     let f = function
       | Syntax.Process p -> Syntax.print_ast p; Format.print_newline () in
     List.iter f ps

let print_ir = function
  | ps -> let f acc x =
            (match x with
             | Syntax.Process p ->
                acc ^ (Machine.string_of_frame (Compile.compile p)) ^ "\n") in
          print_string (List.fold_left f "" ps)

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

let prelude_env = function
  | Syntax.Process p :: [] ->
     let compiled_p = Compile.compile p in
     let final_state = run_full [] [compiled_p] in
     (match final_state with
     | [(_, (_, _, env::env_tail))] -> env
     | _ -> raise (Runtime_error "failed to compile prelude"))
  | _ -> raise (Runtime_error "invalid prelude")    

let exec verbose env = function
  | ps -> let f = function
            | Syntax.Process p -> Compile.compile p in
          let compiled_ps = List.map f ps in
          let print = if verbose then printr_verbose else printr in
          List.iter print (run_full env compiled_ps)  

let get_prelude filename =
  match filename with
  | Some path -> prelude_env (parse_file file_parser path)
  | None -> []

let use_file filename prelude =
  let ps = parse_file file_parser filename in
  if !show_ast then print_ast ps
  else if !show_ir then print_ir ps
  else exec !verbose prelude ps
                               
let toplevel ctx =
  Format.printf "%si, version %s @." name version;
  try
    let _ = ref ctx in
    while true do
      try
        let prelude_env = get_prelude !prelude_path in
        let cmd = read_toplevel toplevel_parser () in
        if !show_ast then print_ast cmd
        else if !show_ir then print_ir cmd
        else exec !verbose prelude_env cmd;
      with
      | Sys.Break -> prerr_endline "Interrupted."
    done
  with End_of_file -> ()

let main () =
  Sys.catch_break true;
  parse_args ();
  Format.set_max_boxes 42 ;
  Format.set_ellipsis_text "..." ;
  let prelude = get_prelude !prelude_path in
  try
    match !file with
    | Some f -> use_file f prelude
    | None -> if !interactive_shell then toplevel ()
  with
    Support.Error err -> Support.print_error err; exit 1

let () = main ()
