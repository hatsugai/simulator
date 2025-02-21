open Printf
open Js_of_ocaml
open Csp
open Config
open Model

type metrics = {
    hilight_color : string;

    left_margin : float;
    top_margin : float;

    min_name_width : float;
    name_box_left_right_margin : float;
    name_box_top_margin : float;
    name_box_bottom_margin : float;
    name_box_line_width : float;

    inter_component_name : float;
    inter_transition : float;
    line_width_transition : float;
    line_width_transition_hilight : float;
    line_width_life_line : float;

    sync_point_radius : float;

    arrow_right_dx0 : float;
    arrow_right_dx1 : float;
    arrow_right_dx2 : float;
    arrow_right_dy0 : float;
    arrow_right_dy1 : float;
    arrow_right_dy2 : float;
    arrow_left_dx0 : float;
    arrow_left_dx1 : float;
    arrow_left_dx2 : float;
    arrow_left_dy0 : float;
    arrow_left_dy1 : float;
    arrow_left_dy2 : float;

    label_left_margin : float;
    trans_inter_label_line :float;
    arrow_space : float;
    erase_under_trans : float;
    trans_line_left_right_margin : float;
    trans_line_back_height : float;
    time_trans_top_margin : float;
    time_trans_bottom_margin : float;
    time_trans_left_margin : float;
    time_trans_right_margin : float;

    label_memo_margin : float;
    memo_left_margin : float;
    memo_top_margin : float;
    memo_right_margin : float;
    memo_bottom_margin : float;
  }

let set_fill_opacity elem opacity =
  elem##setAttribute (Js.string "fill-opacity") (Js.string opacity)

let create_text svg css_class text =
  let elem = Dom_svg.createTextElement Dom_svg.document in
  elem##setAttribute (Js.string "class") (Js.string css_class);
  Js.Unsafe.set elem (Js.string "textContent") (Js.string text);
  Dom.appendChild svg elem;
  let r = elem##getBBox in
  (elem, Js.float_of_number r##.width, Js.float_of_number r##.height)

let create_label ?(hilight=false) svg text =
  create_text svg
    (if hilight then "sequence-label-hilight" else "sequence-label")
    text

let text_size text =
  let r = text##getBBox in (r##.width, r##.height)

let move_text elem x y =
  let sx = sprintf "%f" x in
  let sy = sprintf "%f" y in
  elem##setAttribute (Js.string "x") (Js.string sx);
  elem##setAttribute (Js.string "y") (Js.string sy)

let create_rect svg x y w h lw stroke fill =
  let sx = sprintf "%f" x in
  let sy = sprintf "%f" y in
  let sw = sprintf "%f" w in
  let sh = sprintf "%f" h in
  let slw = sprintf "%f" lw in
  let rect = Dom_svg.createRect Dom_svg.document in
  rect##setAttribute (Js.string "x") (Js.string sx);
  rect##setAttribute (Js.string "y") (Js.string sy);
  rect##setAttribute (Js.string "width") (Js.string sw);
  rect##setAttribute (Js.string "height") (Js.string sh);
  rect##setAttribute (Js.string "fill") (Js.string fill);
  rect##setAttribute (Js.string "stroke") (Js.string stroke);
  rect##setAttribute (Js.string "stroke-width") (Js.string slw);
  Dom.appendChild svg rect;
  rect

let move_rect rect x y w h =
  let sx = sprintf "%f" x in
  let sy = sprintf "%f" y in
  let sw = sprintf "%f" w in
  let sh = sprintf "%f" h in
  rect##setAttribute (Js.string "x") (Js.string sx);
  rect##setAttribute (Js.string "y") (Js.string sy);
  rect##setAttribute (Js.string "width") (Js.string sw);
  rect##setAttribute (Js.string "height") (Js.string sh)

let create_line svg x1 y1 x2 y2 color width dash_opt =
  let sx1 = sprintf "%f" x1 in
  let sy1 = sprintf "%f" y1 in
  let sx2 = sprintf "%f" x2 in
  let sy2 = sprintf "%f" y2 in
  let sw = sprintf "%f" width in
  let line = Dom_svg.createLineElement Dom_svg.document in
  line##setAttribute (Js.string "x1") (Js.string sx1);
  line##setAttribute (Js.string "y1") (Js.string sy1);
  line##setAttribute (Js.string "x2") (Js.string sx2);
  line##setAttribute (Js.string "y2") (Js.string sy2);
  line##setAttribute (Js.string "stroke") (Js.string color);
  line##setAttribute (Js.string "stroke-width") (Js.string sw);
  (match dash_opt with
     None -> ()
   | Some dash ->
      line##setAttribute (Js.string "stroke-dasharray") (Js.string dash));
  Dom.appendChild svg line;
  line

let move_line line x1 y1 x2 y2 =
  let sx1 = sprintf "%f" x1 in
  let sy1 = sprintf "%f" y1 in
  let sx2 = sprintf "%f" x2 in
  let sy2 = sprintf "%f" y2 in
  line##setAttribute (Js.string "x1") (Js.string sx1);
  line##setAttribute (Js.string "y1") (Js.string sy1);
  line##setAttribute (Js.string "x2") (Js.string sx2);
  line##setAttribute (Js.string "y2") (Js.string sy2)

let create_circle svg x y r color =
  let sx = sprintf "%f" x in
  let sy = sprintf "%f" y in
  let sr = sprintf "%f" r in
  let c = Dom_svg.createCircle Dom_svg.document in
  c##setAttribute (Js.string "cx") (Js.string sx);
  c##setAttribute (Js.string "cy") (Js.string sy);
  c##setAttribute (Js.string "r") (Js.string sr);
  c##setAttribute (Js.string "fill") (Js.string color);
  Dom.appendChild svg c

let create_arrow (m : metrics) svg x y color b_dir_right =
  let points =
    if b_dir_right then
      sprintf "%.2f,%.2f %.2f,%.2f %.2f,%.2f"
        (x +. m.arrow_right_dx0)
        (y +. m.arrow_right_dy0)
        (x +. m.arrow_right_dx1)
        (y +. m.arrow_right_dy1)
        (x +. m.arrow_right_dx2)
        (y +. m.arrow_right_dy2)
    else
      sprintf "%.2f,%.2f %.2f,%.2f %.2f,%.2f"
        (x +. m.arrow_left_dx0)
        (y +. m.arrow_left_dy0)
        (x +. m.arrow_left_dx1)
        (y +. m.arrow_left_dy1)
        (x +. m.arrow_left_dx2)
        (y +. m.arrow_left_dy2)
  in
  let poly = Dom_svg.createPolygon Dom_svg.document in
  poly##setAttribute (Js.string "points") (Js.string points);
  poly##setAttribute (Js.string "fill") (Js.string color);
  poly##setAttribute (Js.string "stroke") (Js.string "none");
  Dom.appendChild svg poly

let render_sequence_diagram
      ?(hilight : int option = None)
      config
      (m : metrics)
      (svg : Dom_svg.element Js.t)
      (svg2 : Dom_svg.element Js.t)
      (history : ('label, 'state) history_entry list) =

  let text_elem_list : (Dom_svg.textElement Js.t * float * float) list =
    List.map
      (create_text svg2 "sequence-component-name")
      config.component_name_list
  in
  let name_height : float =
    let (_, _, height) = List.hd text_elem_list in
    height
  in
  let left_x = m.left_margin in
  let inter_compo =
    match config.sequence_diagram_inter_component_opt with
      Some w -> w
    | None -> m.inter_component_name
  in
  let (right_x, compo_x_list) =
      let (x, xs) =
        List.fold_left
          (fun (x, xs) (_, w, _) ->
             let w = max w m.min_name_width in
             let center = x +. m.name_box_left_right_margin +. (w *. 0.5) in
             let right = x +. m.name_box_left_right_margin *. 2.0 +. w in
             (right +. inter_compo, center::xs))
          (left_x, []) text_elem_list
      in
      (x -. inter_compo, List.rev xs)
  in
  let posv = Array.of_list compo_x_list in
  let life_lines =
    List.map
      (fun x ->
        create_line svg x 0.0 x 0.0 "#606060" m.line_width_life_line None)
      compo_x_list
  in
  let y_text = m.top_margin +. m.name_box_top_margin +. name_height in
  List.iter2
    (fun (elem, w, _) x -> move_text elem (x -. w *. 0.5) y_text)
    text_elem_list compo_x_list;
  let h = name_height +. m.name_box_top_margin +. m.name_box_bottom_margin in
  List.iter2
    (fun xc (_, tw, _) ->
      let tw = max tw m.min_name_width in
      let x = xc -. tw *. 0.5 -. m.name_box_left_right_margin in
      let y = m.top_margin in
      let w = tw +. m.name_box_left_right_margin *. 2.0 in
      let _ = create_rect svg2 x y w h m.name_box_line_width "#606060" "none" in
      ())
    compo_x_list text_elem_list;
  let s = sprintf "%f" (m.top_margin +. h +. m.inter_transition) in
  svg2##setAttribute (Js.string "height") (Js.string s);

  let y_start_lifeline = 0.0
  in
  let y_start = 0.0 in

  let render_hist_ent hist_index hist_ent y =
    let hilight = hilight = Some hist_index in
    let hicolor = if hilight then m.hilight_color else "#606060" in
    let hicolorc = if hilight then m.hilight_color else "#606060" in
    let hi_lw =
      if hilight then
        m.line_width_transition_hilight
      else
        m.line_width_transition
    in

    let render_alone label i =
      let rect =
        create_rect svg 0.0 0.0 0.0 0.0 0.0 "none" "white"
      in
      let (text, w, h) =
        create_label ~hilight svg (config.show_label label)
      in
      create_circle svg posv.(i) (y +. h)
        m.sync_point_radius hicolorc;
      let x = posv.(i) +. m.label_left_margin in
      move_rect rect x y w h;
      move_text text x (y +. h);
      (y +. h, x +. w, y +. h)
    in

    let render_communication label i j =
      let rect = create_rect svg 0.0 0.0 0.0 0.0 0.0 "none" "white" in
      let (text, w, h) =
        create_label ~hilight svg (config.show_label label)
      in

      let x1 = min posv.(i) posv.(j) in
      let x2 = max posv.(i) posv.(j) in
      let x =
        if i < j then
          x1 +. m.label_left_margin
        else
          max (x1 +. m.label_left_margin)
            (x2 -. w -. m.label_left_margin)
      in
      let yt = y in
      move_rect rect x yt w (h +. m.erase_under_trans);
      move_text text x (yt +. h);
      let xa = if i < j then posv.(j) -. m.arrow_space else posv.(j) +. m.arrow_space in
      let y = y +. h +. m.trans_inter_label_line in
      let _ = create_rect svg
                 (x1 +. m.trans_line_left_right_margin)
                 (y -. m.trans_line_back_height)
                 (x2 -. x1 -. 2.0 *. m.trans_line_left_right_margin)
                 (m.trans_line_back_height *. 2.0)
                 0.0 "none" "white"
      in
      create_arrow m svg xa y hicolor (i < j);
      let _ = create_line svg posv.(i) y xa y hicolor hi_lw None in
      (y, x +. w, yt +. h)
    in

    let render_multiparty label is =
      let rect = create_rect svg 0.0 0.0 0.0 0.0 0.0 "none" "white" in
      let (text, w, h) =
        create_label ~hilight svg (config.show_label label)
      in

      let i = Util.min_list is in
      let j = Util.max_list is in
      let x1 = posv.(i) in
      let x2 = posv.(j) in
      let x = x1 +. m.label_left_margin in
      let yt = y in
      move_rect rect x yt w (h +. m.erase_under_trans);
      move_text text x (yt +. h);
      let y = y +. h +. m.trans_inter_label_line in
      let _ = create_rect svg
                 (x1 +. m.trans_line_left_right_margin)
                 (y -. m.trans_line_back_height)
                 (x2 -. x1 -. 2.0 *. m.trans_line_left_right_margin)
                 (m.trans_line_back_height *. 2.0)
                 0.0 "none" "white"
      in
      let _ = create_line svg x1 y x2 y hicolor hi_lw None in
      List.iter (fun i ->
          create_circle svg posv.(i) y m.sync_point_radius hicolorc)
        is;
      (y, x +. w, yt +. h)
  in

  let render_event_trans label =
    match hist_ent.party with
      None -> (y, right_x, y)
    | Some party ->
       (match party with
          Alone i -> render_alone label i
         | Communication (i, j) -> render_communication label i j
         | Multiparty is -> render_multiparty label is)
  in

  let render_time_trans delta =
    let rect = create_rect svg 0.0 0.0 0.0 0.0 0.0 "none" "#f0f0f0" in
    let s = sprintf "%d -> %d (+%d)"
              hist_ent.time (hist_ent.time + delta) delta
    in
    let (text, w, h) = create_label ~hilight svg s in
    let width =
      max (w +. m.time_trans_left_margin +. m.time_trans_right_margin)
        (right_x -. left_x)
    in
    let xc = left_x +. 0.5 *. (right_x -. left_x) in
    let xr = xc -. 0.5 *. width in
    let xt = (left_x +. 0.5 *. (right_x -. left_x -. w)) in
    let yt = (y +. h +. m.time_trans_top_margin) in
    move_text text xt yt;
    move_rect rect xr y width
      (h +. m.time_trans_top_margin +. m.time_trans_bottom_margin);
    (y +. h, right_x, yt)
  in

  let render_memo text_x text_y =
    (if hist_ent.memo <> "" then
       begin
         let rect = create_rect svg 0.0 0.0 0.0 0.0 0.0 "none" "#b8ffb8ff" in
         let (text, w, h) = create_text svg "seq-memo" hist_ent.memo in
         let x = text_x +. m.label_memo_margin in
         move_text text (x +. m.memo_left_margin) text_y;
         move_rect rect x (text_y -. h -. m.memo_top_margin)
           (w +. m.memo_left_margin +. m.memo_right_margin)
           (h +. m.memo_top_margin +. m.memo_bottom_margin)
       end)
  in

  let (y, text_x, text_y) =
    match hist_ent.trans_kind with
      EventTrans (label, _) -> render_event_trans label
    | TimeTrans delta -> render_time_trans delta
  in
  render_memo text_x text_y;
  y +. m.inter_transition
  in

  let rec loop hist_index y hist =
    match hist with
      [] -> y
    | hist_ent::hist' ->
       let y = render_hist_ent hist_index hist_ent y in
       loop (hist_index + 1) y hist'
  in
  let y = loop 0 y_start history in
  let s = sprintf "%f" (y +. 100.0) in
  svg##setAttribute (Js.string "height") (Js.string s);
  List.iter2
    (fun line x ->
      move_line line x y_start_lifeline x (y +. m.inter_transition *. 2.0))
    life_lines compo_x_list;
  ()

let init
      (html_elem_id : string)
      (config : ('label, 'state) simulator_config)
      (dch : ('label, 'state) down_message chan)
      ()
  =
  let element : Dom_html.element Js.t = Util.get_elem html_elem_id in
  let svg : Dom_svg.element Js.t =
    match Js.Opt.to_option (Dom_svg.CoerceTo.element element) with
      Some button -> button
    | None -> Error.f "cannot get: %s" html_elem_id
  in

  let element2 : Dom_html.element Js.t = Util.get_elem "seq-header" in
  let svg2 : Dom_svg.element Js.t =
    match Js.Opt.to_option (Dom_svg.CoerceTo.element element2) with
      Some button -> button
    | None -> Error.f "cannot get: %s" html_elem_id
  in

  let device_pixel_ratio = Dom_html.window##.devicePixelRatio in
  printf "device_pixel_ratio: %f\n" (Js.float_of_number device_pixel_ratio);

  let ratio =
    match config.sequence_diagram_ppi_opt with
      Some ppi -> ppi /. 72.0
    | None ->
       let dpi =
         let elem : Dom_html.element Js.t = Util.get_elem "dpi" in
         let h = elem##.offsetHeight in
         printf "dpi: %d\n" h;
         h
       in
       (float dpi) /. 72.0
  in
  printf "ratio = %f\n" ratio;

  let pt_to_px pt = pt *. ratio in

  let metrics = {
    hilight_color = "red";

    left_margin = pt_to_px 30.0;
    top_margin = pt_to_px 20.0;

    min_name_width = pt_to_px 40.0;
    name_box_left_right_margin = pt_to_px 8.0;
    name_box_top_margin = pt_to_px 4.0;
    name_box_bottom_margin = pt_to_px 9.0;
    name_box_line_width = pt_to_px 1.2;

    inter_component_name = pt_to_px 20.0;
    inter_transition = pt_to_px 12.0;
    line_width_transition = pt_to_px 0.8;
    line_width_transition_hilight = pt_to_px 1.2;
    line_width_life_line = pt_to_px 0.5;

    sync_point_radius = pt_to_px 3.0;

    arrow_right_dx0 = pt_to_px 8.0;
    arrow_right_dy0 = pt_to_px 0.0;
    arrow_right_dx1 = pt_to_px (-5.0);
    arrow_right_dy1 = pt_to_px 3.0;
    arrow_right_dx2 = pt_to_px (-5.0);
    arrow_right_dy2 = pt_to_px (-3.0);

    arrow_left_dx0 = pt_to_px (-8.0);
    arrow_left_dy0 = pt_to_px 0.0;
    arrow_left_dx1 = pt_to_px 5.0;
    arrow_left_dy1 = pt_to_px 3.0;
    arrow_left_dx2 = pt_to_px 5.0;
    arrow_left_dy2 = pt_to_px (-3.0);

    label_left_margin = pt_to_px 8.0;
    trans_inter_label_line = pt_to_px 12.0;
    arrow_space = pt_to_px 10.0;
    erase_under_trans = pt_to_px 4.0;
    trans_line_left_right_margin = 4.0;
    trans_line_back_height = 4.0;

    time_trans_top_margin = pt_to_px 0.0;
    time_trans_bottom_margin = pt_to_px 7.0;
    time_trans_left_margin = pt_to_px 6.0;
    time_trans_right_margin = pt_to_px 6.0;

    memo_left_margin = pt_to_px 4.0;
    memo_top_margin = pt_to_px 0.0;
    memo_right_margin = pt_to_px 4.0;
    memo_bottom_margin = pt_to_px 7.0;
    label_memo_margin = pt_to_px 12.0;
  }
  in

  let clear () =
    element##.innerHTML := Js.string "";
    element2##.innerHTML := Js.string ""
  in

  let rec process () =
    recv dch handle

  and handle (Update (_hint, ui_state)) =
    if config.component_name_list = [] then
      process ()
    else
      begin
        clear ();
        UiState.state_case ui_state
          (fun _trans_index_pointed_opt
               hist_index_pointed_opt _hist_index _hist_ent ->
             match hist_index_pointed_opt with
               None ->
               render_sequence_diagram config metrics
                 svg svg2 ui_state.model.history;
               process ()
             | Some hist_index ->
               render_sequence_diagram ~hilight:(Some hist_index) config metrics
                 svg svg2 ui_state.model.history;
               process ())
          (fun trans_index_pointed_opt ->
             match trans_index_pointed_opt with
               None ->
               render_sequence_diagram config metrics
                 svg svg2 ui_state.model.history;
               process ()
             | Some trans_index ->
               let transitions =
                 config.transf
                   ui_state.model.current_state
                   ui_state.model.current_time
               in
               let trans =
                 Util.list_nth __FUNCTION__ transitions trans_index
               in
               let history =
                 {
                   trans_kind = EventTrans (trans.label, trans_index);
                   state = ui_state.model.current_state;
                   time = ui_state.model.current_time;
                   party = Some trans.party;
                   memo = "";
                 }
                 :: ui_state.model.history
               in
               render_sequence_diagram ~hilight:(Some 0)
                 config metrics svg svg2 history;
               process ())
      end
  in

  process ()
