open Printf
open Js_of_ocaml
open Csp
open Config
open Model

let make_advance_buttons uch additional_advance_buttons =
  let elem : Dom_html.element Js.t = Util.get_elem "control-bar" in
  List.map
    (fun delta ->
      let text = sprintf "+%d" delta in
      let dch = make_chan () in
      let process () =
        UcButton.make elem "input" text uch dch
          (AdvanceDelta delta)
          (fun _ -> true)
      in
      (dch, process))
    (List.filter (fun d -> d > 0) additional_advance_buttons)

let setup_win_message_handler uch =
  let on_message event =
    (match Js.Optdef.to_option (Js.Unsafe.get event (Js.string "data")) with
       Some data ->
        Printf.printf "Received message.\n";
        let ui_state : ('label, 'state) UiState.ui_state = data in
        inject uch (Replace ui_state)
     | None ->
        Printf.printf "Received an empty message.\n");
    Js._false
  in
  let _ =
    Dom.addEventListener Dom_html.window
      (Dom.Event.make "message") (Dom.handler on_message) Js._true
  in
  ()

let onload config _ =
  Dom_html.document##.title := Js.string (config.title ^ " - Simulator");

  let uch              = make_chan () in
  let dch_advance      = make_chan () in
  let dch_clone        = make_chan () in
  let dch_cur_time     = make_chan () in
  let dch_history      = make_chan () in
  let dch_load         = make_chan () in
  let dch_memo         = make_chan () in
  let dch_redo         = make_chan () in
  let dch_restart      = make_chan () in
  let dch_save         = make_chan () in
  let dch_sequence     = make_chan () in
  let dch_state        = make_chan () in
  let dch_state_status = make_chan () in
  let dch_target       = make_chan () in
  let dch_time_spec    = make_chan () in
  let dch_trans        = make_chan () in
  let dch_undo         = make_chan () in

  let advance_buttons =
    make_advance_buttons uch config.additional_advance_buttons
  in

  let dch_list = [
      dch_advance;
      dch_clone;
      dch_cur_time;
      dch_history;
      dch_load;
      dch_memo;
      dch_redo;
      dch_restart;
      dch_sequence;
      dch_state;
      dch_state_status;
      dch_target;
      dch_time_spec;
      dch_trans;
      dch_undo;
    ] @ (List.map fst advance_buttons)
  in

  let process_list = [
      Keyhandler.init uch;
      UcTranslist.init "trans" config uch dch_trans;
      UcStateStatus.init "state_status" config dch_state_status;
      UcState.init UcState.CurrentState "state" config uch dch_state;
      UcState.init UcState.TargetState "target" config uch dch_target;
      UcHistory.init "history" config uch dch_history;
      UcRestart.init "restart" uch dch_restart;
      UcLoad.init "load" uch dch_load;
      UcSave.init "save" uch dch_save;
      UcClone.init "clone" uch dch_clone;
      UcUndo.init "undo" uch dch_undo;
      UcRedo.init "redo" uch dch_redo;
      UcCurTime.init "cur_time" dch_cur_time;
      UcTimeSpec.init "time_spec" uch dch_time_spec;
      UcAdvance.init "advance" uch dch_advance;
      UcMemo.init "memo" uch dch_memo;
      UcSequence.init "sequence" config dch_sequence;
      Model.init config uch dch_list
    ]  @ (List.map snd advance_buttons)
  in

  UxVerticalResizer.init "title-history" "left-panel" "container2-1-1";
  UxVerticalResizer.init "title-target" "right-panel" "container2-2-1";
  UxHorizontalResizer1.init "divider1" "main-panel" "left-panel" "right-panel";
  UxHorizontalResizer2.init "divider2" "main-panel" "right-panel";

  setup_win_message_handler uch;

  LocalFile.setup_download (fun bytes -> inject uch (Load bytes));

  (if config.component_name_list = [] then
     Util.hide_elem "container1-3");

  init_csp (fun () -> par process_list);
  Js._false

let f config =
  Dom_html.window##.onload := Dom_html.handler (onload config)
