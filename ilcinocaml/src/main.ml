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
    | _ -> Support.syntax_error ~loc:(Support.location_of_lex lex) "general confusion"

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

let get_prelude filename =
  match filename with
  | Some path -> Ilc.prelude_env (parse_file file_parser path)
  | None -> []

let use_file filename prelude =
  let ps = parse_file file_parser filename in
  if !show_ast then Ilc.print_ast ps
  else if !show_ir then Ilc.print_ir ps
  else Ilc.exec !verbose prelude ps
                               
let toplevel ctx =
  Format.printf "%si, version %s @." name version;
  try
    let _ = ref ctx in
    while true do
      try
        let prelude_env = get_prelude !prelude_path in
        let cmd = read_toplevel toplevel_parser () in
        if !show_ast then Ilc.print_ast cmd
        else if !show_ir then Ilc.print_ir cmd
        else Ilc.exec !verbose prelude_env cmd;
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
