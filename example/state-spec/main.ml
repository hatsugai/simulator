open Printf
open Simulator.Config

type ('event, 'channel, 'state) sync_term =
  Tau of 'state
| Event of 'event * 'state
| Receive of 'channel * ('event -> bool) * ('event -> 'state)
| Hidden of 'event * 'state

module L = struct
  type 'event ievent = Tau | Hid of 'event | ITick
  [@@deriving eq, ord]
  type 'event revent = Vis of 'event | Internal of 'event ievent | Tick
  [@@deriving eq, ord]

  let show_ievent show = function
      Tau -> "tau"
    | Hid e -> "*" ^ (show e)
    | ITick -> "*tick"

  let show_revent show = function
      Vis e -> show e
    | Internal i -> show_ievent show i
    | Tick -> "tick"
end

let update xs i a =
  let rec loop rs k xs =
    match xs with
      [] -> failwith __FUNCTION__
    | x::xs' ->
       if k = 0 then
         List.rev_append rs (a::xs')
       else
         loop (x::rs) (k-1) xs'
  in
  loop [] i xs

let remove_nth xs i =
  let rec loop rs k xs =
    match xs with
      [] -> failwith __FUNCTION__
    | x::xs' ->
       if k = 0 then
         List.rev_append rs xs'
       else
         loop (x::rs) (k-1) xs'
  in
  loop [] i xs

let rec interval a b =
  if a < b then
    a::(interval (a+1) b)
  else
    []

let convert_transf transf channel_to_event_list state _time =
  List.fold_left
    (fun acc syncterm ->
      match syncterm with
        Tau q -> {
          label = L.Internal Tau;
          target = q;
          time_constraint = Enabled None;
          party = Alone 0;
        } :: acc
      | Event (e, q) -> {
          label = Vis e;
          target = q;
          time_constraint = Enabled None;
          party = Alone 0;
        } :: acc
      | Receive (ch, g, f) ->
         List.fold_left
           (fun acc e ->
             if g e then
               {
                 label = L.Vis e;
                 target = f e;
                 time_constraint = Enabled None;
                 party = Alone 0;
               } :: acc
             else
               acc)
           acc (channel_to_event_list ch)
      | Hidden (e, q) -> {
          label = Internal (Hid e);
          target = q;
          time_constraint = Enabled None;
          party = Alone 0;
        } :: acc)
    [] (transf state)
  |> List.rev

module Event =
  struct
    type t =
      CreateAccount
    | DeleteAccount of int
    | Subscribe of int
    | Unsubscribe of int * int
    | Update of int * int
    [@@deriving show { with_path=false }, eq, ord]
  end

module Channel =
  struct
    type t =
      CreateAccount
    | DeleteAccount
    | Subscribe
    | Unsubscribe
    | Update
    [@@deriving show { with_path=false }, eq, ord]
  end

let range = interval 0 4

let channel_to_event_list (ch : Channel.t) : Event.t list =
  match ch with
  | CreateAccount -> [CreateAccount]
  | DeleteAccount -> List.map (fun i -> Event.DeleteAccount i) range
  | Subscribe -> List.map (fun i -> Event.Subscribe i) range
  | Unsubscribe ->
     List.concat_map
       (fun i -> List.map (fun j -> Event.Unsubscribe (i, j)) range) range
  | Update ->
     List.concat_map
       (fun i -> List.map (fun j -> Event.Update (i, j)) range) range

module State = struct
  type t = int list list
  [@@deriving show { with_path=false }, eq, ord]

  let initial_state = []

  [@@@ocaml.warning "-8"]
  let transf xss = [
      Event (Event.CreateAccount, xss @ [[]]);
      Receive (Channel.DeleteAccount,
               (fun (Event.DeleteAccount i) -> i < List.length xss),
               (fun (Event.DeleteAccount i) -> remove_nth xss i));
      Receive (Channel.Subscribe,
               (fun (Event.Subscribe i) -> i < List.length xss),
               (fun (Event.Subscribe i) ->
                 update xss i ((List.nth xss i) @ [0])));
      Receive (Channel.Unsubscribe,
               (fun (Event.Unsubscribe (i, j)) ->
                 i < List.length xss
                 && j < List.length (List.nth xss i)),
               (fun (Event.Unsubscribe (i, j)) ->
                 let xs = List.nth xss i in
                 let xs' = remove_nth xs j in
                 update xss i xs'));
      Receive (Channel.Update,
               (fun (Event.Update (i, j)) ->
                 i < List.length xss
                 && j < List.length (List.nth xss i)),
               (fun (Event.Update (i, j)) ->
                 let xs = List.nth xss i in
                 let x = List.nth xs j in
                 let xs' = update xs j (x+1) in
                 update xss i xs'));
    ]
  [@@@ocaml.warning "+8"]
end

let length1 _pos (xss : State.t) =
  List.length xss

let show_sum _pos (xss : State.t) i =
  if 0 <= i && i < List.length xss then
    sprintf "account %d" i
  else
    ""

let show1 pos (xss : State.t) =
  match pos with
    [i] ->
    if 0 <= i && i < List.length xss then
      sprintf "user_id = %d\nnum_subs = %d\ntotal_updates = %d"
        i
        (List.length (List.nth xss i))
        (List.fold_left (+) 0 (List.nth xss i))
    else ""
  | _ -> ""

let length2 pos (xss : State.t) =
  match pos with
    [i] when 0 <= i && i < List.length xss ->
     List.length (List.nth xss i)
  | _ -> 0

let show2 pos (xss : State.t) j =
  match pos with
    [i] when 0 <= i && i < List.length xss
             && 0 <= j && j < List.length (List.nth xss i) ->
     sprintf "subsc %d" j
  | _ -> ""

let show3 pos (xss : State.t) =
  match pos with
    [i; j] when 0 <= i && i < List.length xss
                && 0 <= j && j < List.length (List.nth xss i) ->
     sprintf "subscription %d\nnum_updates = %d"
       j (List.nth (List.nth xss i) j)
  | _ -> ""

let state_spec (_state : State.t) =
  Hierarchy {
      title_opt = Some "Account";
      length = length1; show = show_sum;
      sub_spec =
        Compound {
            title_opt = None;
            spec_list = [
                Plain {
                    title_opt = None;
                    show = show1;
                  };
                Hierarchy
                  { title_opt = Some "Subscriptions";
                    length = length2; show = show2;
                    sub_spec =
                      Plain { title_opt = None; show = show3 };
                  };
              ];
          }
    }

let () =
  let config = {
      title = "Account Manager";
      state = State.initial_state;
      history = [];
      component_name_list = [];
      transf = convert_transf
                 State.transf channel_to_event_list;
      state_time_constraint_opt = None;
      compare_label_opt = Some (L.compare_revent Event.compare);
      compare_state_opt = Some State.compare;
      show_label = L.show_revent Event.show;
      state_spec;
      additional_advance_buttons = [1; 2; 3; 4; 5; 10; 50; 100];
      sequence_diagram_ppi_opt = None;
      sequence_diagram_inter_component_opt = None;
    }
  in
  Simulator.Init.f config
