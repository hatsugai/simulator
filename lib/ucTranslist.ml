open Printf
open Js_of_ocaml
open Csp
open Config
open Model

type prop = {
    mutable trans_index : int;
    mutable enabled : bool;
}

type translist_state = {
    elem_list : (Dom_html.element Js.t * prop) list;
}

let init
      (html_elem_id : string)
      (config : ('label, 'state) simulator_config)
      (uch : ('label, 'state)up_message chan)
      (dch : ('label, 'state) down_message chan)
      ()
=
  let document = Dom_html.window##.document in
  let element : Dom_html.element Js.t = Util.get_elem html_elem_id in

  let uque : event Queue.t = Queue.create () in
  let ech : ('label, 'state)notification chan = make_chan () in

  let compare_state =
    match config.compare_state_opt with
      Some f -> f
    | None -> (fun _ _ -> 0)
  in

  let compare_label =
    match config.compare_label_opt with
      Some f -> f
    | None ->
       (fun label1 label2 ->
         String.compare (config.show_label label1) (config.show_label label2))
  in

  let compare_time_constraint tc1 tc2 =
    match tc1, tc2 with
      Enabled _, Disabled _ -> (-1)
    | Disabled _, Enabled _ -> 1
    | _ -> 0
  in

  let compare_trans (_, t1) (_, t2) =
    let c = compare_time_constraint t1.time_constraint t2.time_constraint in
    if c <> 0 then c else
      let c = compare_label t1.label t2.label in
      if c <> 0 then c else
        compare_state t1.target t2.target
  in

  let r_trans_index_selected_opt = ref None in


  let on_move prop (_event : Dom_html.mouseEvent Js.t) =
    if prop.enabled then
      inject ech (PointTrans prop.trans_index);
    Js._false
  in

  let on_enter prop (_event : Dom_html.mouseEvent Js.t) =
    if prop.enabled then
      inject ech (PointTrans prop.trans_index);
    Js._false
  in

  let on_leave prop (_event : Dom_html.mouseEvent Js.t) =
    if prop.enabled then
      inject ech Unpoint;
    Js._false
  in

  let on_click prop _event =
    if prop.enabled then
      inject ech (SelectTrans prop.trans_index);
    Js._false
  in

  let make_elem () =
    let prop = {
        trans_index = 0;
        enabled = true;
      }
    in
    let elt = Dom_html.createDiv document in
    elt##.className := Js.string "menu-ent-div";
    elt##.onclick := Dom_html.handler (on_click prop);
    elt##.onmousemove := Dom_html.handler (on_move prop);
    let _= Dom.addEventListener elt (Dom.Event.make "mouseenter")
             (Dom.handler (on_enter prop)) Js._true in
    let _= Dom.addEventListener elt (Dom.Event.make "mouseleave")
             (Dom.handler (on_leave prop)) Js._true in
    Dom.appendChild element elt;
    (elt, prop)
  in

  let set_trans elt prop trans_index_selected_opt trans_index_pointed_opt
        i (trans_index, trans) =
    let style = (Js.Unsafe.coerce elt)##.style in
    style##.display := Js.string "block";
    prop.trans_index <- trans_index;

    let text =
      let label_str = config.show_label trans.label in
      match trans.time_constraint with
        Enabled time_limit_opt ->
         (match time_limit_opt with
            None -> sprintf "%d. %s" i label_str
          | Some time_limit -> sprintf "%d. [≤%d] %s" i time_limit label_str)
      | Disabled time_limit -> sprintf "%d. [<%d] %s" i time_limit label_str
    in
    elt##.textContent := Js.some (Js.string text);
    (match trans.time_constraint with
       Enabled _ ->
        prop.enabled <- true;
        (if Some trans_index = trans_index_selected_opt then
           (elt##.style##.color := Js.string Color.text_selected;
            elt##.style##.backgroundColor := Js.string Color.background_selected)
         else if Some trans_index = trans_index_pointed_opt then
           (elt##.style##.color := Js.string Color.text_pointed;
            elt##.style##.backgroundColor := Js.string Color.background_pointed)
         else
           (elt##.style##.color := Js.string "black";
            elt##.style##.backgroundColor := Js.string "white"))
     | Disabled _ ->
        prop.enabled <- false;
        elt##.style##.color := Js.string Color.background_disabled);
  in

  let set_translist (zzz : translist_state) trans_index_selected_opt
        trans_index_pointed_opt transitions
      : translist_state =
    r_trans_index_selected_opt := trans_index_selected_opt;
    let ps = List.mapi (fun i x -> (i, x)) transitions in
    let ps = List.sort compare_trans ps in
    let rec loop rs i elem_list trans_list =
      match elem_list, trans_list with
        (elem, prop)::elem_list', trans::trans_list' ->
         set_trans elem prop trans_index_selected_opt trans_index_pointed_opt i trans;
         loop ((elem, prop)::rs) (i+1) elem_list' trans_list'
      | [], trans::trans_list' ->
         let (elt, prop) = make_elem () in
         set_trans elt prop trans_index_selected_opt trans_index_pointed_opt i trans;
         loop ((elt, prop)::rs) (i+1) [] trans_list'
      | elem_list, [] ->
         List.iter (fun (elt, _prop) ->
             let style = (Js.Unsafe.coerce elt)##.style in
             style##.display := Js.string "none")
           elem_list;
         {
           elem_list = List.rev_append rs elem_list;
         }
    in
    loop [] 0 zzz.elem_list ps
  in

  let rec process (zzz : translist_state) =
    let event_list = [
        recvEvt dch (handle zzz);
        recvEvt ech (handle_event zzz);
      ]
    in
    Util.select_que event_list uque

  and handle zzz (Update (_hint, ui_state)) =
    let zzz =
      UiState.state_case ui_state
        (fun trans_index_pointed_opt
             _hist_index_pointed_opt _hist_index hist_ent ->
          let transitions = config.transf hist_ent.state hist_ent.time in
          match hist_ent.trans_kind with
            EventTrans (_label, trans_index) ->
             set_translist zzz
               (Some trans_index) trans_index_pointed_opt transitions
          | TimeTrans _delta ->
             set_translist zzz
               None trans_index_pointed_opt transitions)
        (fun trans_index_pointed_opt ->
          let transitions =
            config.transf
              ui_state.model.current_state
              ui_state.model.current_time
          in
          set_translist zzz None trans_index_pointed_opt transitions)
    in
    process zzz

  and handle_event zzz msg =
    request zzz msg; process zzz

  and request zzz msg =
    Queue.add (sendEvt uch msg (uque_drop zzz)) uque;

  and uque_drop zzz () = let _ = Queue.take uque in process zzz

  in

  process { elem_list = [] }
