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

    let is_spawning = function
        | (pid, ([Machine.ISpawn] :: frms, stck, envs)) -> true

    let temp_string_of_process = function
        | (pid, s) -> Printf.sprintf "Process: %d\n %s" pid (Machine.string_of_state s)

    let spawn_all ps = 
        let rec spawn old_ps new_ps = function
            | [] -> if (List.length new_ps) = 0 then (false, List.rev old_ps) else (true, ((List.rev old_ps) @ (List.rev new_ps)))
            | (pid, ([Machine.ISpawn] :: frm1 :: frm2 :: frms, stck, envs)) :: rest_ps ->
                let pid' = !pid_counter in
                pid_counter := !pid_counter + 2;
                spawn ((pid, (frms, stck, envs)) :: old_ps) ((pid'+1, ([frm2], [], envs)) :: (pid', ([frm1], [], envs)) :: new_ps) rest_ps
            | p :: rest_ps -> spawn (p :: old_ps) new_ps rest_ps
        in
        spawn [] [] ps

    let run_all ps = List.map (function | (pid, state) -> Machine.run pid state) ps

    let run_spawn_loop ps =
        let rec loop prev = function
            | (false, ps') -> if prev then loop false (spawn_all (run_all ps')) else ps'
            | (true, ps') -> loop true (spawn_all (run_all ps'))
    in
        loop true (spawn_all ps)
    
    let run ps = 
        let init_ps = Machine.run 0 ([ps], [], [[]]) in
        let rec loop = function
            | (true, ps') -> ps'
            | (_, ps') -> loop (Communication.exec_comm (run_spawn_loop ps'))
        in
            loop (Communication.exec_comm (run_spawn_loop [init_ps]))

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
