open Js_of_ocaml
open Csp
open UiState


type ('label, 'state) notification =
  Restart
| Undo
| Redo
| PointTrans of int
| SelectTrans of int
| Advance
| AdvanceDelta of int
| SetTimeSpec of time_spec
| PointHistoryEntry of int
| SelectHistoryEntry of int
| GoBackToHistoryEntry of int
| ReleaseSelect
| MoveUp
| MoveDown
| Load of bytes
| Save
| Clone
| Replace of ('label, 'state) ui_state
| UpdateMemo of string
| Open
| Unpoint

type ('label, 'state) up_message = ('label, 'state) notification

type hint =
  Hint_StateChanged
| Hint_HistoryChanged
| Hint_TimeSpecChanged
| Hint_PointedChanged
| Hint_HistorySelectionChanged
| Hint_HistoryEntryChanged

type ('label, 'state) down_message =
  Update of hint * ('label, 'state) ui_state

let clone_simulator ui_state =
  let win_opt =
    Dom_html.window##open_ (Js.string "/index.html")
      (Js.string "_blank") Js.Opt.empty
  in
  match Js.Opt.to_option win_opt with
    None ->
     Printf.printf "Failed to open window\n";
     ()
  | Some clone_win ->
     let target_origin = (Js.string "*") in
     let _ =
       Dom_html.window##setTimeout
         (Js.wrap_callback
            (fun () ->
              Js.Unsafe.meth_call clone_win "postMessage"
                [| Js.Unsafe.inject ui_state;
                   Js.Unsafe.inject target_origin |]))
         (Js.number_of_float 100.0)
     in
     ()

let init
      (config : ('label, 'state) Config.simulator_config)
      (uch : ('label, 'state) up_message chan)
      (dch_list : ('label, 'state) down_message chan list)
      ()
  =
  let ui_state = UiState.get_ui_state config in

  let rec process (ui_state : ('label, 'state) ui_state) =
    recv uch (handle_notification ui_state)

  and handle_notification ui_state notification =
    match notification with
      Restart ->
       let ui_state = restart config ui_state in
       changed ui_state Hint_StateChanged process
    | Undo ->
       let ui_state = undo ui_state in
       changed ui_state Hint_StateChanged process
    | Redo ->
       let ui_state = redo ui_state in
       changed ui_state Hint_StateChanged process
    | PointTrans trans_index ->
       let (ui_state, updated) = point_trans ui_state trans_index in
       if updated then
         changed ui_state Hint_PointedChanged process
       else
         process ui_state
    | SelectTrans trans_index ->
       let ui_state = select_trans config ui_state trans_index in
       changed ui_state Hint_StateChanged process
    | Advance ->
       let ui_state =
         match ui_state.time_spec with
           TS_None -> ui_state
         | TS_Absolute time ->
            let cur_time = ui_state.model.current_time in
            if cur_time < time  then
              advance_time ui_state (time - cur_time)
            else
              ui_state
         | TS_Relative delta ->
            advance_time ui_state delta
       in
       changed ui_state Hint_StateChanged process
    | AdvanceDelta delta ->
       let ui_state = advance_time ui_state delta in
       changed ui_state Hint_StateChanged process
    | SetTimeSpec time_spec ->
       let ui_state = set_time_spec ui_state time_spec in
       changed ui_state Hint_TimeSpecChanged process
    | PointHistoryEntry hist_index ->
       let ui_state = point_history_entry ui_state hist_index in
       changed ui_state Hint_PointedChanged process
    | Unpoint ->
       let ui_state = unpoint ui_state in
       changed ui_state Hint_PointedChanged process
    | SelectHistoryEntry hist_index ->
       let ui_state = select_history_entry ui_state hist_index in
       changed ui_state Hint_HistorySelectionChanged process
    | GoBackToHistoryEntry hist_index ->
       let ui_state = go_back_to_history_entry ui_state hist_index in
       changed ui_state Hint_StateChanged process
    | ReleaseSelect ->
       let ui_state = release_select ui_state in
       changed ui_state Hint_HistorySelectionChanged process
    | MoveUp ->
       let ui_state = move_up ui_state in
       changed ui_state Hint_HistorySelectionChanged process
    | MoveDown ->
       let ui_state = move_down ui_state in
       changed ui_state Hint_HistorySelectionChanged process
    | Load marshal_bytes ->
       let ui_state = UiState.save ui_state in
       let ui_state = revive_simulator_state ui_state marshal_bytes in
       changed ui_state Hint_StateChanged process
    | Save ->
       LocalFile.save config.title ui_state.model;
       process ui_state
    | Clone ->
       clone_simulator ui_state;
       process ui_state
    | Replace ui_state ->
       changed ui_state Hint_StateChanged process
    | UpdateMemo memo ->
       state_case ui_state
         (fun _trans_index_pointed_opt
              hist_index_pointed_opt hist_index _hist_ent ->
           if ui_state.history_index_selected_opt <> None
              && hist_index_pointed_opt = None
           then
             let ui_state = update_memo ui_state hist_index memo in
             changed ui_state Hint_HistoryEntryChanged process
           else
             (Printf.printf "UpdateMemo: invalid notification\n";
              process ui_state))
          (fun _trans_index_pointed_opt ->
            if ui_state.model.history <> [] then
              let ui_state = update_memo ui_state 0 memo in
              changed ui_state Hint_HistoryEntryChanged process
            else
              (Printf.printf "UpdateMemo: invalid notification\n";
               process ui_state))
    | Open ->
       LocalFile.open_file_dialog ();
       process ui_state

  and revive_simulator_state ui_state marshal_bytes =
    try
      let (title, model) : (string * ('label, 'state) ModelState.model) =
        Marshal.from_bytes marshal_bytes 0
      in
      if title = config.title then
        {
          ui_state with
          model;
          pointed_state = PointedNone;
          history_index_selected_opt = None;
          time_spec = TS_None;
        }
      else
        let message = Printf.sprintf "wrong save data: \"%s\"" title in
        Js_of_ocaml.Dom_html.window##alert (Js_of_ocaml.Js.string message);
        ui_state
    with
      _ ->
      Js_of_ocaml.Dom_html.window##alert
        (Js_of_ocaml.Js.string "invalid save data");
      ui_state

  and changed ui_state hint p =
    update_notify_loop ui_state hint p dch_list

  and update_notify_loop ui_state hint p chs =
    match chs with
      [] -> p ui_state
    | ch::chs' ->
       send ch (Update (hint, ui_state))
         (fun () -> update_notify_loop ui_state hint p chs')

  in
  changed ui_state Hint_StateChanged process
