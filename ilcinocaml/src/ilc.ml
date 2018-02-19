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
