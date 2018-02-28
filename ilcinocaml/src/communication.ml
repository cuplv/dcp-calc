(* -------------------------------------------------------------------------- *)
(* Message passing *)

open Machine

exception Communication_error of string

let error msg = raise (Communication_error msg)

let imp_lookup x = function
  | _ ::env:: _ -> (try List.assoc x env with Not_found -> error ("unknown " ^ x))
  | _ -> error "no environment to look up implicit arg"              

let is_blocked = function
  | (_, ((IWr _ :: _) :: _, _, _)) -> true
  | (_, ((IRd _ :: _) :: _, _, _)) -> true
  | (_, ((IChoice _ :: _) :: _, _, _)) -> true
  | _ -> false

let is_writing = function
  | (_, IWr _) -> true
  | _ -> false

let is_reading = function
  | (_, IRd _) -> true
  | (_, IChoice _) -> true
  | _ -> false

let get_comm_info = function
  | (pid, ((IWr (v, x) :: _) :: _, _, _)) -> (pid, IWr (v,x))
  | (pid, ((IRd x :: _) :: _, _, envs)) -> (pid, IRd x)
  | (pid, ((IChoice(pid', cid, (IRd x)) :: _) :: _, _, _)) -> (pid, IChoice(pid', cid, IRd x))
  | _ -> error "Process not reading or writing"

let combinations l1 l2 = 
  let res = List.fold_left (fun acc x ->
                List.fold_left (fun acc y -> (x, y) :: acc) acc l2) [] l1 in
  List.rev res

let can_comm = function
  | ((_, IWr (_, c)), (_, IRd c')) when c=c' -> true
  | ((_, IWr (_, c)), (_, (IChoice (_, _, IRd c')))) when c=c' -> true
  | _ -> false

(* TODO: Generalize reads! *)
(* TODO: equality for closures *)
let update_state comm = function
  | (pid, ((IRd x1 :: is) :: frms, stck, envs)) as s ->
     (match comm with
      | ((pid1, IWr (v, x)), (pid2, IRd x1'))
           when (pid=pid2 && x1=x1') ->
         [(pid, (is :: frms, v :: stck, envs))]
      | _ -> [s])
  | (pid, ((IChoice (cpid, cid, (IRd x1)) :: is) :: frms, stck, envs)) ->
     (match comm with
      | ((pid1, IWr (v, x)), (pid2, IChoice(cpid', cid', IRd x1')))
           when (pid=pid2 && x1=x1' && cpid=cpid' && cid=cid') ->
         [(pid, (is :: frms, v :: stck, envs))]
      | ((pid1, IWr (v, x)), (pid2, IChoice(cpid', cid', IRd x1'))) ->
         [(pid, ((IBlock(IRd x1') :: is) :: frms, stck, envs))]
      | _ -> [(pid, ((IRd x1 :: is) :: frms, stck, envs))])
  (* Wr *)
  | (pid, ((IWr (MClosure (n, f, e),x) :: is) :: frms, stck, envs)) as s ->
     (match comm with
      | ((pid', IWr (MClosure (n',f',e'), x')), _)
           when (pid=pid' && n=n' && x=x') ->
         [(pid, (is :: frms, stck, envs))]
      | _ -> [s])
  | (pid, ((IWr (v,x) :: is) :: frms, stck, envs)) as s ->
     (match comm with
      | ((pid', IWr (MClosure _, x')), _) ->
         [(pid, ((IWr (v,x) :: is) :: frms, stck, envs))]
      | ((pid', IWr (v', x')), _)
           when (pid=pid' && v=v' && x=x') ->
         [(pid, (is :: frms, stck, envs))]
      | _ -> [s])
  | (p, state) as s -> [s]

let run_comm ps =
  let open List in
  let comm_ps = map get_comm_info (filter is_blocked ps) in
  let possible_comms =
    filter can_comm
           (combinations (filter is_writing comm_ps) (filter is_reading comm_ps)) in
  let halted = length possible_comms = 0 in 
  if halted then (halted, ps)
  else (halted, fold_left (fun acc x -> (update_state (hd possible_comms) x) @
                                          acc ) [] (rev ps))
