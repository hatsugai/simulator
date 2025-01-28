open Js_of_ocaml
open Csp
open Model

let init
      (html_elem_id : string)
      (uch : ('label, 'state) up_message chan)
      (dch : ('label, 'state) down_message chan)
      ()
=
  let element : Dom_html.element Js.t = Util.get_elem html_elem_id in
  let input : Dom_html.inputElement Js.t =
    match Js.Opt.to_option (Dom_html.CoerceTo.input element) with
      Some input -> input
    | None -> Error.f "cannot get element: %s" html_elem_id
  in

  let uque : event Queue.t = Queue.create () in
  let ech : ('label, 'state)notification chan = make_chan () in

  let rec process () =
    let event_list = [
        recvEvt dch handle;
        recvEvt ech handle_event;
      ]
    in
    Util.select_que event_list uque

  and handle_event msg =
    request msg; process ()

  and handle (Update (_hint, ui_state)) =
    (UiState.state_case ui_state
       (fun _trans_index_pointed_opt
            hist_index_pointed_opt _hist_index hist_ent ->
         input##.value := Js.string hist_ent.memo;
         input##.disabled :=
           if ui_state.history_index_selected_opt <> None
              && hist_index_pointed_opt = None
           then Js._false else Js._true)
       (fun _trans_index_pointed_opt ->
         match ui_state.model.history with
           [] ->
            input##.textContent := Js.some (Js.string "");
            input##.disabled := Js._true
         | hist_ent::_history' ->
            input##.value := Js.string hist_ent.memo;
            input##.disabled := Js._false));
    process ()

  and request msg =
    Queue.add (sendEvt uch msg uque_drop) uque

  and uque_drop () = let _ = Queue.take uque in process ()

  in

  let on_input _event =
    let memo = Js.to_string input##.value in
    inject ech (UpdateMemo memo);
    Js._false
  in

  input##.oninput := Dom.handler on_input;

  process ()
