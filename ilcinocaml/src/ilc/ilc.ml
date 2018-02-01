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

    let string_of_process = function
        | (pid, ([], [v], e)) -> Printf.sprintf "%s%d:\n%s\n" "process" pid (Print.string_of_mvalue v)
        | (pid, (frm :: frms, _, _)) -> Printf.sprintf "%s%d:\n%s" "process" pid (Print.string_of_frame frm)
        | (pid, ([] , [], e)) -> Printf.sprintf "%s%d:\n0\n" "process" pid 
        | _ -> raise (Process_error "illegal final state")

    let pid_counter = ref 1

    let temp_string_of_process = function
        | (pid, s) -> Printf.sprintf "Process: %d\n %s" pid (Machine.string_of_state s)

    let spawn_all ps = 
        let rec spawn old_ps new_ps = function
            | [] -> if (List.length new_ps) = 0 then (true, (List.rev old_ps)) else (false, ((List.rev old_ps) @ (List.rev new_ps)))
            | (pid, ([Machine.ISpawn] :: frm1 :: frm2 :: frms, stck, envs)) :: rest_ps ->
                let pid' = !pid_counter in
                pid_counter := !pid_counter + 2;
                spawn ((pid, (frms, stck, envs)) :: old_ps) ((pid'+1, ([frm2], [], envs)) :: (pid', ([frm1], [], envs)) :: new_ps) rest_ps
            | p :: rest_ps -> spawn (p :: old_ps) new_ps rest_ps
        in
        spawn [] [] ps

    let is_hole = function
        | (_, ((Machine.IHole _ :: _) :: _, _, _)) -> true
        | _ -> false

    let fill_hole ps = function
        | (pid, ((Machine.IHole n :: is ) :: frms, stck, envs)) ->
            let rec check_ps = function
                | (pid', ([], [v], _)) :: _ when n=pid' -> (true, (pid, (is :: frms, v :: stck, envs)))
                | (pid', _) :: _ when n=pid' -> (false, (pid, ((Machine.IHole n :: is ) :: frms, stck, envs)))
                | _ :: rest -> check_ps rest
                | [] -> raise (Process_error "process not found")
            in
            check_ps ps
        | _ -> raise (Process_error "not a hole")

    let fill_all ps =
        let rec loop acc filled = function
            | [] -> (not filled, List.rev acc)
            | p :: ps' -> if (is_hole p) then
                let (was_filled, new_p) = fill_hole ps p in
                loop (new_p :: acc) was_filled ps'
                          else loop (p :: acc) filled ps'
        in loop [] false ps

    let run_all ps = List.map (function | (pid, state) -> Machine.run pid state) ps

    let run_everything ps =  
        let quit_loop = ref false in
        let prev_done_spawning = ref false in
        let prev_done_filling = ref false in
        let hold_ps = ref ps in
        while not !quit_loop do
            let (done_spawning, ps') = spawn_all (run_all !hold_ps) in
            let (done_filling, ps') = fill_all (run_all ps') in
            quit_loop := done_spawning && !prev_done_spawning &&
                         done_filling && !prev_done_filling;
            prev_done_spawning := done_spawning;
            prev_done_filling := done_filling;
            hold_ps := ps';
        done; !hold_ps

    let run ps = 
        let init_ps = Machine.run 0 ([ps], [], [[]]) in
        let rec loop = function
            | (true, ps') -> ps'
            | (_, ps') -> loop (Communication.exec_comm (run_everything ps'))
        in
            loop (Communication.exec_comm (run_everything [init_ps]))

    let exec env = function
        | Syntax.Process p ->
            (* Print ast *)
            (*print_endline (Print.string_of_expr p); env*)

            (* Print IR *)
            (*let instrs = Compile.compile p in
            print_endline (Print.string_of_frame instrs); env*)

            let ps = Compile.compile p in
            List.map print_endline (List.map string_of_process (run ps)); env
end) ;;

ILC.main ()
