open Printf
open Js_of_ocaml
open Csp
open Model

let init
      (html_elem_id : string)
      (dch : ('label, 'state) down_message chan)
      ()
  =
  let uque : event Queue.t = Queue.create () in

  let element : Dom_html.element Js.t = Util.get_elem html_elem_id in

  let input : Dom_html.inputElement Js.t =
    match Js.Opt.to_option (Dom_html.CoerceTo.input element) with
      Some input -> input
    | None -> Error.f "cannot get %s" html_elem_id
  in

  let display_time time =
    let s = sprintf "%d" time in
    input##.value := Js.string s
  in

  let rec process () =
    let event_list = [
        recvEvt dch handle;
      ]
    in
    Util.select_que event_list uque

  and handle (Update (_hint, ui_state)) =
    UiState.state_case ui_state
      (fun _trans_index_pointed_opt
           _hist_index_pointed_opt _hist_index hist_ent ->
        display_time hist_ent.time)
      (fun _trans_index_pointed_opt ->
        display_time ui_state.model.current_time);
    process ()

  in

  process ()
