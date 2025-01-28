exception Error of string

type 'a link =
  Nil
| Link of {
    mutable next : 'a link;
    mutable prev : 'a link;
    data : 'a;
  }

type 'a t = {
    mutable head : 'a link;
    mutable tail : 'a link;
  }

type event = {
    pollFn : unit -> bool;
    doFn : unit -> unit;
    blockFn : bool ref -> unit;
  }

type process_state = RUNNING | DORMANT | OMEGA

type 'a chan = {
    chid : int;
    sendQ : (bool ref * process_desc * 'a * process) t;
    mutable recv
            : (bool ref * process_desc * ('a -> bool) * ('a -> command)) option;
  }

and command =
  Skip
| Sync of event list
| Par of process list
| Seq of process list

and process = unit -> command

and process_desc = {
    mutable state : process_state;
    mutable parent : process_desc option;
    mutable children : process_desc array;
    mutable num_alives : int;
    mutable cont : process;
    mutable seq : process list;
    mutable seq_stack : process list list;
}

let create () = { head = Nil; tail = Nil }

let add_tail q x =
  let p = Link { next = Nil; prev = q.tail; data = x } in
  match q.tail with
    Nil -> (q.head <- p; q.tail <- p)
  | Link r -> (r.next <- p; q.tail <- p)

let del_link q p =
  match p with
    Nil -> ()
  | Link r ->
     (match r.next with
        Nil -> ()
      | Link s -> s.prev <- r.prev);
     (match r.prev with
        Nil -> ()
      | Link s -> s.next <- r.next);
     (if q.head == p then q.head <- r.next);
     (if q.tail == p then q.tail <- r.prev)

let chid ch = ch.chid

let make_process parent thunk = {
    state = RUNNING;
    parent = parent;
    children = [||];
    num_alives = 1;
    cont = thunk;
    seq = [];
    seq_stack = [];
}

let dummy_process = make_process None (fun () -> Skip)
let cur_process = ref dummy_process
let rdyQ : process_desc Queue.t = Queue.create ()
let next_chid = ref 0

let error msg =
  raise (Error msg)

let make_chan () =
  let chid = !next_chid in
  next_chid := !next_chid + 1;
  { chid = chid; sendQ = create (); recv = None }

let rec propagate p_opt =
  match p_opt with
    None -> ()
  | Some p ->
     p.num_alives <- p.num_alives - 1;
     if p.num_alives = 0 then
       (match p.seq with
          [] ->
           (match p.seq_stack with
              [] ->
               p.state <- OMEGA;
               propagate p.parent
            | s::ss ->
               p.seq_stack <- ss;
               (match s with
                  [] -> error "propagate"
                | k::ks ->
                   p.cont <- k;
                   p.seq <- ks;
                   Queue.add p rdyQ))
        | k::ks ->
           p.cont <- k;
           p.seq <- ks;
           Queue.add p rdyQ)

let rec find_fire_event_opt event_list =
  match event_list with
    [] -> None
  | e::es' ->
     if e.pollFn () then
       Some e
     else
       find_fire_event_opt es'

let block event_list =
  let dirtyFlg = ref false in
  let block e = e.blockFn dirtyFlg in
  List.iter block event_list

let dispatch cmd =
  match cmd with
    Skip -> propagate (Some !cur_process)
  | Sync es ->
     (match find_fire_event_opt es with
        None -> block es
      | Some e -> e.doFn ())
  | Par ts ->
     let n = List.length ts in
     let v = Array.make n !cur_process in
     List.iteri
       (fun i thunk ->
         let p = make_process (Some !cur_process) thunk in
         v.(i) <- p;
         Queue.add p rdyQ)
       ts;
     !cur_process.state <- DORMANT;
     !cur_process.children <- v;
     !cur_process.num_alives <- n
  | Seq ts ->
     (match ts with
        [] ->
         propagate (Some !cur_process)
      | k::ks ->
         (if !cur_process.seq <> [] then
            !cur_process.seq_stack <- !cur_process.seq::!cur_process.seq_stack);
         !cur_process.seq <- ks;
         !cur_process.cont <- k;
         Queue.add !cur_process rdyQ)

let rec drive () =
  if Queue.is_empty rdyQ then
    ()
  else
    let p = Queue.take rdyQ in
    cur_process := p;
    let cmd = p.cont () in
    dispatch cmd;
    drive ()

let init_csp thunk =
  Queue.clear rdyQ;
  let p = make_process None thunk in
  Queue.add p rdyQ;
  drive ()

let sendEvt ch msg k =
  let sender = !cur_process in
  let pollFn () =
    match ch.recv with
      None -> false
    | Some (flg, _p, g, _k) ->
       if !flg then
         (ch.recv <- None; false)
       else
         g msg
  and doFn () =
    match ch.recv with
      None -> error "sendEvt doFn"
    | Some (flg, receiver, _g, recvK) ->
       flg := true;
       sender.cont <- k;
       Queue.add sender rdyQ;
       receiver.cont <- (fun () -> recvK msg);
       Queue.add receiver rdyQ;
       ch.recv <- None
  and blockFn flg =
    add_tail ch.sendQ (flg, sender, msg, k)
  in
  { pollFn = pollFn; doFn = doFn; blockFn = blockFn }

let recvEvt ?(g = (fun _ -> true)) ch k =
  let receiver = !cur_process in
  let cache = ref Nil in
  let pollFn () =
    let rec pollQ p =
      match p with
        Nil ->
         false
      | Link r ->
         let (flg, _sender, msg, _k) = r.data in
         if !flg then
           let p' = r.next in
           del_link ch.sendQ p;
           pollQ p'
         else if g msg then
           (cache := p; true)
         else
           pollQ r.next
    in
    pollQ ch.sendQ.head
  and doFn () =
    match !cache with
      Nil -> error "recvEvt doFn"
    | Link r ->
       let (flg, sender, msg, senderK) = r.data in
       flg := true;
       sender.cont <- senderK;
       Queue.add sender rdyQ;
       receiver.cont <- (fun () -> k msg);
       Queue.add receiver rdyQ;
       ch.recv <- None
  and blockFn flg =
    ch.recv <- Some (flg, receiver, g, k)
  in
  { pollFn = pollFn; doFn = doFn; blockFn = blockFn }

let select event_list = Sync event_list
let par process_list = Par process_list
let seq process_list = Seq process_list
let sync event = select [event]
let send ch msg k = sync (sendEvt ch msg k)
let recv ?(g = fun _ -> true) ch k = sync (recvEvt ~g ch k)
let skip () = Skip

let inject ch msg =
  match ch.recv with
    None -> error "inject"
  | Some (flg, receiver, g, k) ->
     if !flg || not (g msg) then
       error "inject"
     else
       begin
         ch.recv <- None;
         flg := true;
         cur_process := receiver;
         !cur_process.cont <- (fun () -> k msg);
         Queue.add !cur_process rdyQ;
         drive ()
       end
