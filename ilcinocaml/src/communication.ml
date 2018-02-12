(* -------------------------------------------------------------------------- *)
(* Message passing *)

open Machine

exception Communication_error of string

let error msg = raise (Communication_error msg) 

let is_blocked = function
  | (_, ((IWr _ :: _) :: _, _, _)) -> true
  | (_, ((IRdBind _ :: _) :: _, _, _)) -> true
  | (_, ((IRd _ :: _) :: _, _, _)) -> true
  | (_, ((IChoice _ :: _) :: _, _, _)) -> true
  | (_, ((IRepl _ :: _) :: _, _, _)) -> true
  | _ -> false

let is_writing = function
  | (_, IWr _) -> true
  | _ -> false

let is_reading = function
  | (_, IRdBind _) -> true
  | (_, IRd _) -> true
  | (_, IChoice _) -> true
  | (_, IRepl _) -> true (* Replication must be in read mode *)
  | _ -> false

let get_comm_info = function
  | (pid, ((IWr (v, x) :: _) :: _, _, _)) -> (pid, IWr (v,x))
  | (pid, ((IRdBind (x1, x2) :: _) :: _, _, _)) -> (pid, IRdBind (x1,x2))
  | (pid, ((IRd x :: _) :: _, _, _)) -> (pid, IRd x)
  | (pid, ((IChoice(pid', cid, (IRdBind (x1, x2))) :: _) :: _, _, _)) ->
      (pid, IChoice(pid', cid, IRdBind (x1,x2)))
  | (pid, ((IChoice(pid', cid, (IRd x)) :: _) :: _, _, _)) -> (pid, IChoice(pid', cid, IRd x))
  | (pid, ((IRepl frm :: _) :: _, _, _)) -> (pid, IRepl frm)
  | _ -> error "Process not reading or writing"

let combinations l1 l2 = 
  let res = List.fold_left (fun acc x ->
    List.fold_left (fun acc y -> (x, y) :: acc) acc l2) [] l1 in
  List.rev res

let can_comm = function
  | ((_, IWr (_, c)), (_, IRdBind (_, c'))) when c=c' -> true
  | ((_, IWr (_, c)), (_, (IChoice (_, _, IRdBind (_, c'))))) when c=c' -> true
  | ((_, IWr (_, c)), (_, (IRepl (IRdBind (_, c') :: _)))) when c=c' -> true
  | ((_, IWr (_, c)), (_, IRd c')) when c=c' -> true
  | ((_, IWr (_, c)), (_, (IChoice (_, _, IRd c')))) when c=c' -> true
  | ((_, IWr (_, c)), (_, (IRepl (IRd c' :: _)))) when c=c' -> true
  | _ -> false

(* TODO: Generalize reads! *)
(* TODO: equality for closures *)
let update_state comm = function
  (* Rd w/ bind *)
  | (pid, ((IRdBind (x1,x2) :: is) :: frms, stck, env :: envs)) ->
      (match comm with
      | ((pid1, IWr (v, x)), (pid2, IRdBind (x1', x2')))
          when (pid=pid2 && x1=x1' && x2=x2') ->
            [(pid, (is :: frms, stck, ((x1,v) :: env) :: envs))]
      | _ -> [(pid, ((IRdBind (x1,x2) :: is) :: frms, stck, env :: envs))])
  | (pid, ((IRepl (IRdBind (x1,x2) :: rest_frm) :: is) :: frms, stck, env :: envs)) ->
      (match comm with
      | ((pid1, IWr (v, x)), (pid2, IRepl (IRdBind (x1', x2') :: rest_frm')))
          when (pid=pid2 && x1=x1' && x2=x2') ->
            let new_pid = !pid_counter in
            incr pid_counter;
            [(pid, (rest_frm :: is :: frms, stck, ((x1,v) :: env) :: envs));
             (new_pid, ((IRepl (IRdBind (x1,x2) :: rest_frm) :: is) :: frms, stck, env :: envs))]
      | _ -> [(pid, ((IRepl (IRdBind (x1,x2) :: rest_frm) :: is) :: frms, stck, env :: envs))])
  | (pid, ((IChoice (cpid, cid, (IRdBind (x1,x2))) :: is) :: frms, stck, env :: envs)) ->
      (match comm with
      | ((pid1, IWr (v, x)), (pid2, IChoice(cpid', cid', IRdBind (x1', x2'))))
          when (pid=pid2 && x1=x1' && x2=x2' && cpid=cpid' && cid=cid') ->
            [(pid, (is :: frms, stck, ((x1,v) :: env) :: envs))]
      | ((pid1, IWr (v, x)), (pid2, IChoice(cpid', cid', IRdBind (x1', x2')))) ->
          [(pid, ((IBlock(IRdBind (x1', x2')) :: is) :: frms, stck, env :: envs))]
      | _ -> [(pid, ((IRdBind (x1,x2) :: is) :: frms, stck, env :: envs))])
  (* Rd w/o bind *)
  | (pid, ((IRd x1 :: is) :: frms, stck, envs)) ->
      (match comm with
      | ((pid1, IWr (v, x)), (pid2, IRd x1'))
          when (pid=pid2 && x1=x1') ->
            [(pid, (is :: frms, v :: stck, envs))]
      | _ -> [(pid, ((IRd x1 :: is) :: frms, stck, envs))])
  | (pid, ((IRepl (IRd x1 :: rest_frm) :: is) :: frms, stck, envs)) ->
      (match comm with
      | ((pid1, IWr (v, x)), (pid2, IRepl (IRd x1' :: rest_frm')))
          when (pid=pid2 && x1=x1') ->
            let new_pid = !pid_counter in
            incr pid_counter;
            [(pid, (rest_frm :: is :: frms, v :: stck, envs));
            (new_pid, ((IRepl (IRd x1' :: rest_frm) :: is) :: frms, stck, envs))]
      | _ -> [(pid, ((IRepl (IRd x1 :: rest_frm) :: is) :: frms, stck, envs))])
  | (pid, ((IChoice (cpid, cid, (IRd x1)) :: is) :: frms, stck, envs)) ->
      (match comm with
      | ((pid1, IWr (v, x)), (pid2, IChoice(cpid', cid', IRd x1')))
          when (pid=pid2 && x1=x1' && cpid=cpid' && cid=cid') ->
            [(pid, (is :: frms, v :: stck, envs))]
      | ((pid1, IWr (v, x)), (pid2, IChoice(cpid', cid', IRd x1'))) ->
          [(pid, ((IBlock(IRd x1') :: is) :: frms, stck, envs))]
      | _ -> [(pid, ((IRd x1 :: is) :: frms, stck, envs))])
  (* Wr *)
  | (pid, ((IWr (MClosure (n, f, e),x) :: is) :: frms, stck, envs)) ->
      (match comm with
      | ((pid', IWr (MClosure (n',f',e'), x')), _)
          when (pid=pid' && n=n' && x=x') ->
            [(pid, (is :: frms, stck, envs))]
      | _ -> [(pid, ((IWr (MClosure (n,f,e),x) :: is) :: frms, stck, envs))])
  | (pid, ((IWr (v,x) :: is) :: frms, stck, envs)) ->
      (match comm with
      | ((pid', IWr (MClosure _, x')), _) ->
          [(pid, ((IWr (v,x) :: is) :: frms, stck, envs))]
      | ((pid', IWr (v', x')), _)
          when (pid=pid' && v=v' && x=x') ->
            [(pid, (is :: frms, stck, envs))]
      | _ -> [(pid, ((IWr (v,x) :: is) :: frms, stck, envs))])
  | (p, state) -> [(p, state)]

let run_comm ps =
  let open List in
  let comm_ps = map get_comm_info (filter is_blocked ps) in
  let possible_comms = filter can_comm (combinations
    (filter is_writing comm_ps) (filter is_reading comm_ps)) in
  let halted = length possible_comms = 0 in 
  if halted then (halted, ps)
  else (halted, fold_left (fun acc x -> (update_state (hd possible_comms) x) @
        acc ) [] (rev ps))
