open Printf
open Js_of_ocaml
open Csp
open Config
open Model

type history_state = {
    elem_list : Dom_html.element Js.t list;
}

let init
      (html_elem_id : string)
      (config : ('label, 'state) simulator_config)
      (uch : ('label, 'state) up_message chan)
      (dch : ('label, 'state) down_message chan)
      ()
=
  let document = Dom_html.window##.document in
  let element : Dom_html.element Js.t = Util.get_elem html_elem_id in

  let uque : event Queue.t = Queue.create () in
  let ech : ('label, 'state)notification chan = make_chan () in

  let r_history_index_selected_opt = ref None in

  let on_enter hist_index (_event : Dom_html.mouseEvent Js.t) =
    inject ech (PointHistoryEntry hist_index);
    Js._false
  in

  let on_leave (_event : Dom_html.mouseEvent Js.t) =
    inject ech Unpoint;
    Js._false
  in

  let hilight history_index_selected_opt history_index_pointed_opt index div =
    if Some index = history_index_selected_opt then
      (div##.style##.color := Js.string Color.text_selected;
       div##.style##.backgroundColor := Js.string Color.background_selected)
    else if Some index = history_index_pointed_opt then
      (div##.style##.color := Js.string Color.text_pointed;
       div##.style##.backgroundColor := Js.string Color.background_pointed)
    else
      (div##.style##.color := Js.string "";
       div##.style##.backgroundColor := Js.string "")
  in

  let rec on_click index (_ : Dom_html.mouseEvent Js.t) =
    inject ech (SelectHistoryEntry index);
    Js._false

  and on_dblclick index (_ : Dom_html.mouseEvent Js.t) =
    inject ech (GoBackToHistoryEntry index);
    Js._false

  and make_elem i =
    let elem = Dom_html.createDiv document in
    elem##.onclick := Dom_html.handler (on_click i);
    elem##.ondblclick := Dom_html.handler (on_dblclick i);
    let _ = Dom.addEventListener elem (Dom.Event.make "mouseenter")
              (Dom.handler (on_enter i)) Js._true in
    let _ = Dom.addEventListener elem (Dom.Event.make "mouseleave")
              (Dom.handler on_leave) Js._true in
    Dom.appendChild element elem;
    elem

  and set_ent elem hist_ent n i history_index_selected_opt history_index_pointed_opt =
    let style = (Js.Unsafe.coerce elem)##.style in
    style##.display := Js.string "block";
    (if hist_ent.memo = "" then
       elem##.className := Js.string "history-ent-div"
     else
       elem##.className := Js.string "history-ent-div-hilight");
    hilight history_index_selected_opt history_index_pointed_opt i elem;
    let text =
      match (hist_ent.trans_kind : 'label trans_kind) with
        EventTrans (label, _trans_index) ->
         if hist_ent.memo = "" then
           sprintf "%d. [%d] %s" (n-i-1) hist_ent.time (config.show_label label)
         else
           sprintf "%d. [%d] %s @ %s" (n-i-1) hist_ent.time
             (config.show_label label) hist_ent.memo
      | TimeTrans delta ->
         if hist_ent.memo = "" then
           sprintf "%d. [%d] +%d -> %d" (n-i-1) hist_ent.time delta
             (hist_ent.time + delta)
         else
           sprintf "%d. [%d] +%d -> %d @ %s" (n-i-1) hist_ent.time delta
             (hist_ent.time + delta) hist_ent.memo
    in
    elem##.textContent := Js.some (Js.string text)
  in

  let cons ui_state zzz history_index_selected_opt history_index_pointed_opt =
    r_history_index_selected_opt := history_index_selected_opt;
    let history = ui_state.UiState.model.history in
    let n = List.length history in
    let rec loop rs i elem_list history =
      match elem_list, history with
        elem::elem_list', hist_ent::history' ->
         set_ent elem hist_ent n i history_index_selected_opt history_index_pointed_opt;
         loop (elem::rs) (i+1) elem_list' history'
      | [], hist_ent::history' ->
         let elem = make_elem i in
         set_ent elem hist_ent n i history_index_selected_opt history_index_pointed_opt;
         loop (elem::rs) (i+1) [] history'
      | elem_list, [] ->
         List.iter (fun elem ->
             let style = (Js.Unsafe.coerce elem)##.style in
             style##.display := Js.string "none")
           elem_list;
         {
           elem_list = List.rev_append rs elem_list;
         }
    in
    loop [] 0 zzz.elem_list history
  in

  let rec process (zzz : history_state) =
    let event_list = [
        recvEvt dch (handle zzz);
        recvEvt ech (handle_event zzz);
      ]
    in
    Util.select_que event_list uque

  and handle_event zzz msg =
    request zzz msg; process zzz

  and handle zzz (Update (_hint, ui_state)) =
    let history_index_pointed_opt =
      match ui_state.pointed_state with
        PointedHistoryEntry hist_index -> Some hist_index
      | _ -> None
    in
    let zzz =
      cons ui_state zzz
        ui_state.history_index_selected_opt history_index_pointed_opt
    in
    process zzz

  and request zzz msg =
    Queue.add (sendEvt uch msg (uque_drop zzz)) uque

  and uque_drop zzz () = let _ = Queue.take uque in process zzz

  in

  process { elem_list = [] }
