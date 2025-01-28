open Js_of_ocaml

let init
      (id_tracker : string)
      (id_parent : string)
      (id_child1 : string)
      (id_child2 : string) =

  let tracker : Dom_html.element Js.t = Util.get_elem id_tracker in
  let parent : Dom_html.element Js.t = Util.get_elem id_parent in
  let child1 : Dom_html.element Js.t = Util.get_elem id_child1 in
  let child2 : Dom_html.element Js.t = Util.get_elem id_child2 in

  let capture = ref false in
  let ehdr_id_ref_up = ref None in
  let ehdr_id_ref_move = ref None in
  let x0 = ref 0 in
  let w_sum = ref 0.0 in

  let on_move event =
    if !capture then
      begin
        let rect = parent##getBoundingClientRect in
        let w1 = (float_of_int event##.clientX) -. rect##.left -. (float_of_int !x0) in
        let w2 = !w_sum -. w1 in
        child1##.style##.width :=
          Js.string (Printf.sprintf "%.0fpx" w1);
        child2##.style##.width :=
          Js.string (Printf.sprintf "%.0fpx" w2);
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
    x0 := event##.offsetX;
    let r1 = child1##getBoundingClientRect in
    let r2 = child2##getBoundingClientRect in
    w_sum := Js.Optdef.get r1##.width (fun () -> 0.0)
         +. Js.Optdef.get r2##.width (fun () -> 0.0);
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
