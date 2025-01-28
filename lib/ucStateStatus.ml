open Printf
open Js_of_ocaml
open Csp
open Config
open Model

let init
      (html_elem_id : string)
      (config : ('label, 'state) simulator_config)
      (dch : ('label, 'state) down_message chan)
      ()
  =
  let element : Dom_html.element Js.t = Util.get_elem html_elem_id in

  let rec process () =
    recv dch handle

  and handle (Update (_hint, ui_state)) =
    (match config.state_time_constraint_opt with
       None -> ()
     | Some state_time_constraint ->
        let (state, time) =
          UiState.state_case ui_state
            (fun _trans_index_pointed_opt
                 _hist_index_pointed_opt _hist_index hist_ent ->
              (hist_ent.state, hist_ent.time))
            (fun _trans_index_pointed_opt ->
              (ui_state.model.current_state, ui_state.model.current_time))
        in
        (match state_time_constraint state time with
           None ->
            element##.textContent := Js.some (Js.string "Current State");
            element##.className := Js.string "section-title"
         | Some time_limit ->
            let text = sprintf "Current State [≤%d]" time_limit in
            element##.textContent := Js.some (Js.string text);
            if time <= time_limit then
              element##.className := Js.string "section-title"
            else
              element##.className := Js.string "section-title-error"));
    process ()

  in

  process ()
