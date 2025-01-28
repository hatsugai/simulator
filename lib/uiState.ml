open Config

type pointed_state =
  PointedNone
| PointedTrans of int
| PointedHistoryEntry of int

type time_spec = TS_None | TS_Absolute of int | TS_Relative of int

type ('label, 'state) states_to_save = {
    model : ('label, 'state) ModelState.model;
    history_index_selected_opt : int option;
}

type ('label, 'state) ui_state = {
    undo : ('label, 'state) states_to_save list;
    redo : ('label, 'state) states_to_save list;
    model : ('label, 'state) ModelState.model;
    time_spec : time_spec;
    history_index_selected_opt : int option;
    pointed_state : pointed_state;
  }

let get_ui_state config =
  let model = ModelState.get_model config in
  {
    model;
    pointed_state = PointedNone;
    history_index_selected_opt = None;
    time_spec = TS_None;
    undo = [];
    redo = [];
  }


let undoable (ui_state : ('label, 'state) ui_state) =
  ui_state.undo <> []

let redoable (ui_state : ('label, 'state) ui_state) =
  ui_state.redo <> []

let undo (ui_state : ('label, 'state) ui_state) =
  match ui_state.undo with
    [] -> ui_state
  | prev::undo' ->
     let cur : ('label, 'state) states_to_save = {
         model = ui_state.model;
         history_index_selected_opt = ui_state.history_index_selected_opt;
       }
     in
     { ui_state with
       model = prev.model;
       history_index_selected_opt = prev.history_index_selected_opt;
       pointed_state = PointedNone;
       undo = undo';
       redo = cur::ui_state.redo;
     }

let redo (ui_state : ('label, 'state) ui_state) =
  match ui_state.redo with
    [] -> ui_state
  | next::redo' ->
     let cur : ('label, 'state) states_to_save = {
         model = ui_state.model;
         history_index_selected_opt = ui_state.history_index_selected_opt;
       }
     in
     { ui_state with
       model = next.model;
       history_index_selected_opt = next.history_index_selected_opt;
       pointed_state = PointedNone;
       redo = redo';
       undo = cur::ui_state.undo;
     }

let save ui_state =
  let cur : ('label, 'state) states_to_save = {
      model = ui_state.model;
      history_index_selected_opt = ui_state.history_index_selected_opt;
    }
  in
  { ui_state with
    undo = cur::ui_state.undo;
    redo = [];
  }


let restart config (ui_state : ('label, 'state) ui_state) =
  let ui_state = save ui_state in
  {
    ui_state with
    model = ModelState.reset config;
    pointed_state = PointedNone;
    history_index_selected_opt = None;
  }

let select_trans config (ui_state : ('label, 'state) ui_state) (i : int) =
  let ui_state = save ui_state in
  let model =
    match ui_state.history_index_selected_opt with
      Some history_index ->
       ModelState.go_back_to_history_entry ui_state.model history_index
    | None -> ui_state.model
  in
  {
    ui_state with
    model = ModelState.select config model i;
    history_index_selected_opt = None;
    pointed_state = PointedNone;
  }

let advance_time ui_state time_delta =
  let ui_state = save ui_state in
  let model =
    match ui_state.history_index_selected_opt with
      Some history_index ->
       ModelState.go_back_to_history_entry ui_state.model history_index
    | None -> ui_state.model
  in
  {
    ui_state with
    model = ModelState.advance_time model time_delta;
    pointed_state = PointedNone;
    history_index_selected_opt = None;
  }

let select_history_entry ui_state hist_index =
  { ui_state with history_index_selected_opt = Some hist_index }

let release_select ui_state =
  {
    ui_state with
    history_index_selected_opt = None;
    pointed_state = PointedNone;
  }

let go_back_to_history_entry ui_state hist_index =
  let ui_state = save ui_state in
  let model = ModelState.go_back_to_history_entry ui_state.model hist_index in
  {
    ui_state with
    model;
    pointed_state = PointedNone;
    history_index_selected_opt = None;
  }

let point_history_entry ui_state hist_index =
  { ui_state with pointed_state = PointedHistoryEntry hist_index }

let unpoint ui_state =
  { ui_state with pointed_state = PointedNone }

let point_trans ui_state trans_index =
  if ui_state.pointed_state = PointedTrans trans_index then
    (ui_state, false)
  else
    let ui_state =
      { ui_state with pointed_state = PointedTrans trans_index }
    in
    (ui_state, true)

let set_time_spec ui_state time_spec =
  { ui_state with time_spec = time_spec }

let move_up ui_state =
  match ui_state.history_index_selected_opt with
    Some history_index ->
     if history_index > 0 then
       {
         ui_state with
         history_index_selected_opt = Some (history_index - 1);
         pointed_state = PointedNone;
       }
     else
       ui_state
  | None -> ui_state

let move_down ui_state =
  let n = List.length ui_state.model.history in
  match ui_state.history_index_selected_opt with
    Some history_index ->
     if history_index < n - 1 then
       {
         ui_state with
         history_index_selected_opt = Some (history_index + 1);
         pointed_state = PointedNone;
       }
     else
       ui_state
  | None -> ui_state

let update_memo ui_state hist_index memo =
  let ui_state = save ui_state in
  let model = ModelState.update_memo ui_state.model hist_index memo in
  { ui_state with model }

let state_case ui_state
      (f_hist : int option -> int option -> int ->
                ('label, 'state) history_entry -> 'a)
      (f_cur : int option -> 'a)
    : 'a =
    let (hist_index_pointed_opt, trans_index_pointed_opt) =
      match ui_state.pointed_state with
        PointedNone -> (None, None)
      | PointedTrans trans_index -> (None, Some trans_index)
      | PointedHistoryEntry hist_index -> (Some hist_index, None)
    in
    if ui_state.history_index_selected_opt <> None
       || hist_index_pointed_opt <> None then
      let hist_index_opt =
        if hist_index_pointed_opt <> None then
          hist_index_pointed_opt
        else
          ui_state.history_index_selected_opt
      in
      let hist_index = Option.get hist_index_opt in
      let hist_ent =
        Util.list_nth __FUNCTION__ ui_state.model.history hist_index
      in
      f_hist trans_index_pointed_opt hist_index_pointed_opt hist_index hist_ent
    else
      f_cur trans_index_pointed_opt

let pointed_state_is_trans ui_state =
  match ui_state.pointed_state with
    PointedTrans _ -> true | _ -> false

let pointed_state_is_history ui_state =
  match ui_state.pointed_state with
    PointedHistoryEntry _ -> true | _ -> false

