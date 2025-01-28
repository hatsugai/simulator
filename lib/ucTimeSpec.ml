open Scanf
open Js_of_ocaml
open Csp
open Model

let allowed_key_list =
  [ "ArrowLeft"; "ArrowRight"; "Backspace"; "Delete";
    "Home"; "End" ]

let is_digit (c : char) : bool =
  Char.compare c '0' >= 0 && Char.compare c '9' <= 0

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
    | None -> Error.f "cannot get time_spec"
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

  and handle _msg =
    process ()

  and request msg =
    Queue.add (sendEvt uch msg uque_drop) uque

  and uque_drop () = let _ = Queue.take uque in process ()

  in

  let set_time_spec () =
    let s = Js.to_string input##.value in
    if String.length s >= 2 && s.[0] = '+' then
      sscanf s "+%d" (fun k -> inject ech (SetTimeSpec (TS_Relative k)))
    else if String.length s > 0 && String.for_all is_digit s then
      sscanf s "%d" (fun k -> inject ech (SetTimeSpec (TS_Absolute k)))
    else
      inject ech (SetTimeSpec (TS_None))
  in

  let advance () =
    inject ech Advance
  in

  let on_input _event =
    set_time_spec ();
    Js._false
  in

  let on_keydown (event : Dom_html.keyboardEvent Js.t) =
    let s = Js.to_string input##.value in
    let pos = input##.selectionStart in
    let key =
      Js.to_string (Js.Optdef.get event##.key (fun () -> Js.string ""))
    in
    if String.equal key "Enter"  then
      (advance (); Js._false)
    else if String.length key = 1 then
      let c = key.[0] in
      if is_digit c || c='+' && pos = 0 && String.for_all is_digit s then
        Js._true
      else
        Js._false
    else if List.mem key allowed_key_list then
      Js._true
    else
      Js._false
  in

  input##.onkeydown := Dom.handler on_keydown;
  input##.oninput := Dom.handler on_input;

  process ()
