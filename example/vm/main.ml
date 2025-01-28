open Printf
open Simulator.Config

type ('event, 'channel, 'a) channel = {
    chid : 'channel;
    extract : 'event -> 'a;
    make : 'a -> 'event;
  }

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

let rec update_alist alist key value =
  match alist with
    [] -> failwith __FUNCTION__
  | (k, v)::alist' ->
     if k=key then
       (k, value)::alist'
     else
       (k, v)::(update_alist alist' key value)

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

type drink = Coffee | Tea | Orange | Apple | Beer
[@@deriving show { with_path=false }, eq, ord]

module Event =
  struct
    type t = Coin of int
           | Light of drink list
           | Btn of drink
           | Out of drink
           | Return of int
           | Eject
    [@@deriving show { with_path=false }, eq, ord]
  end

module Channel =
  struct
    type t = Coin | Light | Btn | Out | Return | Eject
    [@@deriving show { with_path=false }, eq, ord]
  end

let drink_list = [Coffee; Tea; Orange; Apple; Beer]
let coin_list = [10; 50; 100]
let drink_price_list = [
    (Coffee, 150); (Tea, 150); (Orange, 120); (Apple, 120); (Beer, 500)]

let coin : (Event.t, Channel.t, int) channel = {
    chid = Coin;
    extract = (fun (Coin x) -> x) [@ocaml.warning "-8"];
    make = (fun x -> Coin x);
}

let btn : (Event.t, Channel.t, drink) channel = {
    chid = Btn;
    extract = (fun (Btn x) -> x) [@ocaml.warning "-8"];
    make = (fun x -> Btn x);
}

let channel_to_event_list (chid : Channel.t) : Event.t list =
  match chid with
    Coin -> List.map (fun x -> Event.Coin x) coin_list
  | Light -> []
  | Btn -> List.map (fun x -> Event.Btn x) drink_list
  | Out -> List.map (fun x -> Event.Out x) drink_list
  | Return -> []
  | Eject -> []

let recv ?(g = (fun _ -> true))
      (ch : ('event, 'channel, 'a) channel)
      (f : 'a -> 'state)
    : ('event, 'channel, 'state) sync_term =
  let g' event = g (ch.extract event) in
  let f' event = f (ch.extract event) in
  Receive (ch.chid, g', f')

let price drink =
  List.assoc drink drink_price_list

module State = struct
  type t =
    Ready of { stock : (drink * int) list }
  | S1 of { stock : (drink * int) list; amount : int; count : int }
  | Inserting of { stock : (drink * int) list; amount : int; count : int }
  | Selected of { stock : (drink * int) list; change : int; drink : drink }
  | S4 of { stock : (drink * int) list; change : int; drink : drink }
  | Returning of { stock : (drink * int) list; change : int }
  | S6 of { stock : (drink * int) list }
  [@@deriving show { with_path=false }, eq, ord]

  let get_stock drink stock = List.assoc drink stock
  let update_stock drink stock =
    update_alist stock drink (List.assoc drink stock - 1)

  let initial_state =
    Ready { stock = [
          (Coffee, 10); (Tea, 8); (Orange, 3); (Apple, 5); (Beer, 0);
      ]}

  let transf state =
    match state with
      Ready { stock } ->
       [ recv coin (fun c -> S1 { stock; amount = c; count = 1}) ]
    | S1 { stock; amount; count } ->
       [
         let ds =
           List.filter
             (fun drink ->
               get_stock drink stock > 0
               && price drink <= amount)
             drink_list
         in
         Event (Light ds, Inserting { stock; amount; count })
       ]
    | Inserting { stock; amount; count } -> [
        recv coin
          (fun c -> S1 { stock; amount = amount + c; count = count + 1});
        recv btn
          ~g: (fun drink ->
            get_stock drink stock > 0 && price drink <= amount)
          (fun drink ->
            let stock = update_stock drink stock
            and change = amount - price drink
            in
            Selected { stock; change; drink});
        Event (Eject, Returning { stock; change = amount});
      ]
    | Selected { stock; change; drink } ->
       [ Event (Light [drink], S4 { stock; change; drink }) ]
    | S4  { stock; change; drink } ->
       [ Event (Out drink, Returning { stock; change }) ]
    | Returning { stock; change } ->
       [ Event (Return change, S6 { stock }) ]
    | S6 { stock } ->
       [ Event (Light [], Ready { stock }) ]
end

let print_stock buf stock =
  List.iter
    (fun (drink, count) ->
      bprintf buf "  %s: %d\n" (show_drink drink) count)
    stock

let print_state buf (state : State.t) =
  match state with
    Ready { stock } ->
     bprintf buf "Ready\n";
     print_stock buf stock
  | S1 { stock; amount; count } ->
     bprintf buf "S1\n";
     print_stock buf stock;
     bprintf buf "amount = %d\n" amount;
     bprintf buf "count = %d\n" count
  | Inserting { stock; amount; count } ->
     bprintf buf "Inserting\n";
     print_stock buf stock;
     bprintf buf "amount = %d\n" amount;
     bprintf buf "count = %d\n" count
  | Selected { stock; change; drink } ->
     bprintf buf "Selected\n";
     print_stock buf stock;
     bprintf buf "change = %d\n" change;
     bprintf buf "drink = %s\n" (show_drink drink)
  | S4  { stock; change; drink } ->
     bprintf buf "S4\n";
     print_stock buf stock;
     bprintf buf "change = %d\n" change;
     bprintf buf "drink = %s\n" (show_drink drink)
  | Returning { stock; change } ->
     bprintf buf "Returning\n";
     print_stock buf stock;
     bprintf buf "change = %d\n" change
  | S6 { stock } ->
     bprintf buf "S5\n";
     print_stock buf stock

let show_state (state : State.t) =
  let buf = Buffer.create 0 in
  print_state buf state;
  Buffer.contents buf

let state_spec (_state : State.t) =
  Plain { title_opt = None;
          show = (fun _pos state -> show_state state) }

let () =
  let config = {
      title = "Vending Machine";
      state = State.initial_state;
      history = [];
      component_name_list = [];
      transf = convert_transf State.transf channel_to_event_list;
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
