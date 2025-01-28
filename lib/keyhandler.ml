open Js_of_ocaml
open Csp
open Model

let is_shortcut event key shortcut =
  String.equal key shortcut
  && (event##.ctrlKey = Js._true
      || event##.metaKey = Js._true)

let init
      (uch : ('label, 'state) up_message chan)
      ()
  =
  let uque : event Queue.t = Queue.create () in
  let ech : ('label, 'state)notification chan = make_chan () in
  let rec process () =
    let event_list = [
        recvEvt ech handle_event;
      ]
    in
    Util.select_que event_list uque

  and handle_event msg =
    request msg; process ()

  and request msg =
    Queue.add (sendEvt uch msg uque_drop) uque;

  and uque_drop () = let _ = Queue.take uque in process ()

  in

  let on_key_down (event : Dom_html.keyboardEvent Js.t) =
    let key =
      Js.to_string (Js.Optdef.get event##.key (fun () -> Js.string ""))
    in
    (if String.equal key "Escape" then
       (inject ech ReleaseSelect; Js._false)
     else if String.equal key "ArrowUp" then
       (inject ech MoveUp; Js._false)
     else if String.equal key "ArrowDown" then
       (inject ech MoveDown; Js._false)
     else if is_shortcut event key "z" then
       (inject ech Undo; Js._false)
     else if is_shortcut event key "y" then
       (inject ech Redo; Js._false)
     else if is_shortcut event key "o" then
       (inject ech Open; Js._false)
     else if is_shortcut event key "s" then
       (inject ech Save; Js._false)
     else
       Js._true)
  in

  Dom_html.window##.onkeydown := Dom_html.handler on_key_down;

  process ()
