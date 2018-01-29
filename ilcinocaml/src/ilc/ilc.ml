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

    let is_blocked = function
        | (pid, (Machine.IWr _ :: _) :: _, _, _) -> true
        | (pid, (Machine.IRd _ :: _) :: _, _, _) -> true
        | _ -> false

    exception Omg
    exception Wtf of string
    let get_pids = function
        | (pid, (Machine.IWr (v, x) :: _) :: _, _, _) -> (pid, Machine.IWr (v,x))
        | (pid, (Machine.IRd (x1, x2) :: _) :: _, _, _) -> (pid, Machine.IRd (x1,x2))
        | _ -> raise Omg

    let string_of_pid = function
        | (pid, instr) -> Printf.sprintf "(%d,%s)" pid (Machine.string_of_instr instr)

    let string_of_state = function
        | (pid, [], [v], e) -> Printf.sprintf "%s%d:\n%s\n" "process" pid (Machine.string_of_mvalue v)
        | (pid, frm :: frms, _, _) -> Printf.sprintf "%s%d:\n%s" "process" pid (Compile.string_of_frame frm)
        | (pid, [] , [], e) -> Printf.sprintf "%s%d:\n0\n" "process" pid 


    (* TODO: remove redundant pairs *)
	let combinations l1 l2 = 
	   let res = List.fold_left (fun acc x ->
		 List.fold_left (fun acc y -> (x, y) :: acc) acc l2
	   ) [] l1 in
	   List.rev res

	let can_comm = function
        | ((pid1, Machine.IWr (v, x)), (pid2, Machine.IRd (x1, x2))) -> if x = x2 then true else false
        | ((pid1, Machine.IRd (x1, x2)), (pid2, Machine.IWr (v, x))) -> if x = x2 then true else false
        | _ -> false

    let string_of_pair = function
        | ((pid1, instr1), (pid2, instr2)) ->
                Printf.sprintf "(%d,%s), (%d,%s)" pid1 (Machine.string_of_instr instr1) pid2 (Machine.string_of_instr instr2)

    exception Msg_error of string

    let send_comm msg = function
        | (pid, (Machine.IRd (x1,x2) :: is) :: frms, stck, env :: envs) ->
            (match msg with
            | ((pid1, Machine.IWr (v, x)), (pid2, Machine.IRd (x1', x2')))
                when (pid=pid2 && x1=x1' && x2=x2') ->
                    (pid, is :: frms, stck, ((x1,v) :: env) :: envs)
            | ((pid1, Machine.IRd (x1', x2')), (pid2, Machine.IWr (v, x)))
                when (pid=pid1 && x1=x1' && x2=x2') ->
                    (pid, is :: frms, stck, ((x1,v) :: env) :: envs)
            | _ -> (pid, (Machine.IRd (x1,x2) :: is) :: frms, stck, env :: envs))
        | (pid, (Machine.IWr (v,x) :: is) :: frms, stck, envs) ->
            (match msg with
            | ((pid', Machine.IWr (v', x')), _)
                when (pid=pid' && v=v' && x=x') ->
                    (pid, is :: frms, stck, envs)
            | (_, (pid', Machine.IWr (v', x')))
                when (pid=pid' && v=v' && x=x') ->
                    (pid, is :: frms, stck, envs)
            | _ -> (pid, (Machine.IWr (v,x) :: is) :: frms, stck, envs))
        | (pid, frms, stck, envs) -> (pid, frms, stck, envs)

    (* TODO: Change this to use optional arguments *)
    let rec run_processes counter acc = function
        | [] -> acc
        | frm :: rest_frms -> run_processes (counter + 1) ((Machine.run counter frm [] []) :: acc) rest_frms

    let rec continue_processes = function
        | (pid, frms, stcks, envs) -> Machine.continue pid frms stcks envs

    let rec string_of_processes ps = List.map string_of_state ps

    (*let full_run p =
        let processes = split (Compile.compile p) in
        let ran_processes = run_processes 0 [] processes in
        let comm_processes = List.filter is_blocked ran_processes in
        let abbrev_processes = List.map get_pids comm_processes in
        let process_pairs = combinations abbrev_processes abbrev_processes in
        let comm_pairs = List.filter can_comm process_pairs in
        (process_pairs, List.map (send_comm (List.hd comm_pairs)) ran_processes)*)

    let full_run ps =
        let comm_processes = List.filter is_blocked ps in
        let abbrev_processes = List.map get_pids comm_processes in
        let process_pairs = combinations abbrev_processes abbrev_processes in
        let comm_pairs = List.filter can_comm process_pairs in
        (try (process_pairs, List.map (send_comm (List.hd comm_pairs)) ps)
        with Failure "hd" -> (process_pairs, ps))

    let rec fuller_run = function
        | ([], ps) -> ps
        | (_, ps) -> fuller_run (full_run (List.map continue_processes ps))

    let exec env =  function
        | Syntax.Process p ->
            let processes = split (Compile.compile p) in
            let ran_processes = List.rev (run_processes 0 [] processes) in
            List.map print_endline (string_of_processes (fuller_run (full_run ran_processes))); env
            (*List.map print_endline (string_of_processes (List.rev (full_run p))); env*)
            (*let processes = split (Compile.compile p) in
            let ran_processes = run_processes 0 [] processes in
            let comm_processes = List.filter is_blocked ran_processes in
            let abbrev_processes = List.map get_pids comm_processes in
            let process_pairs = combinations abbrev_processes abbrev_processes in
            let comm_pairs = List.filter can_comm process_pairs in
            let updated_processes = List.map (send_comm (List.hd comm_pairs)) ran_processes in
            List.map print_endline (string_of_processes (List.rev updated_processes)); env*)

            (*List.map print_endline (List.map string_of_pair comm_pairs);
            env*)
            (*List.map print_endline (List.map string_of_pid (List.map get_pids (List.filter is_blocked ran_processes))); env*)
            (*List.map print_endline (string_of_processes (List.rev (get_comms ran_processes))); env*)
end) ;;

ILC.main ()
