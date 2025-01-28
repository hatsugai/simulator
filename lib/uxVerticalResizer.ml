open Js_of_ocaml

let init
      (id_tracker : string)
      (id_parent : string)
      (id_child : string) =

  let tracker : Dom_html.element Js.t = Util.get_elem id_tracker in
  let parent : Dom_html.element Js.t = Util.get_elem id_parent in
  let child : Dom_html.element Js.t = Util.get_elem id_child in

  let capture = ref false in
  let ehdr_id_ref_up = ref None in
  let ehdr_id_ref_move = ref None in
  let y0 = ref 0 in

  let on_move event =
    if !capture then
      begin
        let rect = parent##getBoundingClientRect in
        let new_size = (float_of_int event##.clientY) -. rect##.top -. (float_of_int !y0) in
        child##.style##.height :=
          Js.string (Printf.sprintf "%.0fpx" new_size);
        Js._false
      end
    else
      Js._false
  in

  let on_up _event =
    if !capture then
      begin
        capture := false;
        Dom.removeEventListener (Option.get !ehdr_id_ref_up);
        Dom.removeEventListener (Option.get !ehdr_id_ref_move)
      end;
    Js._false
  in

  let on_down (event : Dom_html.mouseEvent Js.t) =
    y0 := event##.offsetY;
    capture := true;
    ehdr_id_ref_up :=
      Some (Dom.addEventListener
              Dom_html.window
              (Dom.Event.make "mouseup")
              (Dom.handler on_up)
              Js._true);
    ehdr_id_ref_move :=
      Some (Dom.addEventListener
              Dom_html.window
              (Dom.Event.make "mousemove")
              (Dom.handler on_move)
              Js._true);
    Js._false
  in

  tracker##.onmousedown := Dom.handler on_down
