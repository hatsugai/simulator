open Js_of_ocaml
open Csp

type button_event = Pushed

let init2
      (button : Dom_html.buttonElement Js.t)
      (uch : 'b chan)
      (dch : 'a chan)
      (msg_on_pushed : 'b)
      (pred_enabled : 'a -> bool)
  =
  let uque : event Queue.t = Queue.create () in
  let ech : button_event chan = make_chan () in

  let rec process () =
    let event_list = [
        recvEvt dch handle;
        recvEvt ech handle_event;
      ]
    in
    Util.select_que event_list uque

  and handle_event _msg =
    request msg_on_pushed; process ()

  and handle msg =
    (if pred_enabled msg then
       button##.disabled := Js._false
     else
       button##.disabled := Js._true);
    process ()

  and request msg =
    Queue.add (sendEvt uch msg uque_drop) uque

  and uque_drop () = let _ = Queue.take uque in process ()

  in

  let onclick _event =
    inject ech Pushed;
    Js._false
  in

  button##.onclick := Dom_html.handler onclick;
  process ()

let make
      (element : Dom_html.element Js.t)
      (css_class : string)
      (name : string)
      (uch : 'b chan)
      (dch : 'a chan)
      (msg_on_pushed : 'b)
      (pred_enabled : 'a -> bool)
  =
  let button = Dom_html.createButton Dom_html.document in
  Dom.appendChild element button;
  button##.className := Js.string css_class;
  button##.textContent := Js.some (Js.string name);
  init2 button uch dch msg_on_pushed pred_enabled

let init
      (html_elem_id : string)
      (uch : 'b chan)
      (dch : 'a chan)
      (msg_on_pushed : 'b)
      (pred_enabled : 'a -> bool)
      ()
  =
  let element : Dom_html.element Js.t = Util.get_elem html_elem_id in

  let button : Dom_html.buttonElement Js.t =
    match Js.Opt.to_option (Dom_html.CoerceTo.button element) with
      Some button -> button
    | None -> Error.f "cannot get button: %s" html_elem_id
  in
  init2 button uch dch msg_on_pushed pred_enabled
