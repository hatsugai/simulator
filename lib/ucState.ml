open Printf
open Js_of_ocaml
open Csp
open Config
open Model

module IntMap = Util.IntMap

type state_selection = CurrentState | TargetState

module SpecSkeleton = struct
  type t =
    Plain of string option
  | Compound of string option * t list
  | List of string option
  | Hierarchy of int * string option * t
  [@@deriving show { with_path = false }, eq, ord]
end

let extract_spec_skeleton (spec : 'state spec) : SpecSkeleton.t =
  let next_id = ref 0 in
  let rec extract (spec : 'state spec) : SpecSkeleton.t =
    match spec with
      Plain { title_opt; show = _ } -> Plain title_opt
    | Compound { title_opt; spec_list } ->
       Compound (title_opt, List.map extract spec_list)
    | List { title_opt; length = _; show = _ } ->
       List title_opt
    | Hierarchy { title_opt; sub_spec; length = _; show = _ } ->
       let id = !next_id in
       next_id := id + 1;
       Hierarchy (id, title_opt, extract sub_spec)
  in
  extract spec

module SpecMap = Map.Make (SpecSkeleton)

let document : Dom_html.document Js.t = Dom_html.window##.document

let class_state_line = "state-line"
let class_state_line_hilighted = "state-line-hilighted"

let cons_diff_div (elem : Dom_html.element Js.t) (text1 : string) (text2 : string) =
  let add_div b_same text =
    let div = Dom_html.createDiv document in
    Dom.appendChild elem div;
    div##.textContent := Js.some (Js.string text);
    div##.className :=
      Js.string (if b_same then class_state_line
                 else class_state_line_hilighted)
  in

  let rec loop xs ys =
    match xs, ys with
      [], [] -> ()
    | x::xs', y::ys' ->
       add_div (String.equal x y) y;
       loop xs' ys'
    | [], ys ->
       List.iter (add_div false) ys
    | _, [] ->
       add_div false "(missing)"
  in
  let lines1 = Util.split_lines text1 in
  let lines2 = Util.split_lines text2 in
  loop lines1 lines2

let r_sel_map : int IntMap.t ref = ref IntMap.empty
let r_spec_skel_map : int IntMap.t SpecMap.t ref = ref SpecMap.empty

let init
      (state_selection : state_selection)
      (html_elem_id : string)
      (config : ('label, 'state) simulator_config)
      (uch : ('label, 'state)up_message chan)
      (dch : ('label, 'state) down_message chan)
      ()
  =

  let element : Dom_html.element Js.t = Util.get_elem html_elem_id in

  let uque : event Queue.t = Queue.create () in
  let ech : ('label, 'state)notification chan = make_chan () in

  let spec_nil = SpecSkeleton.Plain None in
  let r_spec_skel : SpecSkeleton.t ref = ref spec_nil in

  let add_title parent title_opt =
    match title_opt with
      None -> ()
    | Some title ->
       let div_title = Dom_html.createDiv document in
       div_title##.className := Js.string "section-title";
       div_title##.textContent := Js.some (Js.string title);
       Dom.appendChild parent div_title
  in

  let add_sep elem =
    let div_sep = Dom_html.createDiv document in
    Dom.appendChild elem div_sep;
    div_sep##.className := Js.string "separator"
  in

  let rec onclick base_opt state spec skel hier_id index _event =
    r_sel_map := IntMap.add hier_id index !r_sel_map;
    r_spec_skel_map := SpecMap.add skel !r_sel_map !r_spec_skel_map;
    construct ~base_opt state spec skel;
    Js._false

  and construct ?(base_opt = None) (state : 'state) (spec0 : 'state spec) (skel0 : SpecSkeleton.t) =

    let rec cons (elem : Dom_html.element Js.t) (spec : 'state spec) (skel : SpecSkeleton.t) pos =
      match spec, skel with
        Plain { title_opt; show }, _ ->
         let div_show = Dom_html.createDiv document in
         Dom.appendChild elem div_show;
         div_show##.className := Js.string "state-compo";
         add_title div_show title_opt;
         let div_plain = Dom_html.createDiv document in
         Dom.appendChild div_show div_plain;
         div_plain##.className := Js.string "show-plain vscroll";
         let text = show pos state in
         (match base_opt with
            None ->
             div_plain##.textContent := Js.some (Js.string text)
          | Some base ->
             let text2 = show pos base in
             cons_diff_div div_plain text2 text)

      | Compound { title_opt; spec_list }, Compound (_, skel_list) ->
         let div_show = Dom_html.createDiv document in
         Dom.appendChild elem div_show;
         div_show##.className := Js.string "state-compo";
         add_title div_show title_opt;
         let div_compound = Dom_html.createDiv document in
         Dom.appendChild div_show div_compound;
         div_compound##.className := Js.string "show-compound";
         (match spec_list, skel_list with
            spec1::spec_list', skel1::skel_list' ->
             cons div_compound spec1 skel1 pos;
             List.iter2
               (fun spec skel ->
                 add_sep div_compound;
                 cons div_compound spec skel pos)
               spec_list' skel_list'
          | _ -> ())

      | List { title_opt; length; show }, _ ->
         let div_show = Dom_html.createDiv document in
         Dom.appendChild elem div_show;
         div_show##.className := Js.string "state-compo";
         add_title div_show title_opt;
         let div_list = Dom_html.createDiv document in
         Dom.appendChild div_show div_list;
         div_list##.className := Js.string "show-list vscroll";
         let n = length pos state in
         for i=0 to n-1 do
           let d = Dom_html.createDiv document in
           let str = show pos state i in
           let class_name =
             match base_opt with
               Some base ->
                let str2 = show pos base i in
                if str = str2 then
                  "menu-ent-div"
                else
                  "menu-ent-div-hilighted"
             | None -> "menu-ent-div"
           in
           d##.className := Js.string class_name;
           let text = sprintf "%d. %s" i str in
           d##.textContent := Js.some (Js.string text);
           Dom.appendChild div_list d
         done

      | Hierarchy { title_opt; sub_spec; length; show },
        Hierarchy (id, _, sub_skel) ->
         let n = length pos state in
         let sel_index =
           match IntMap.find_opt id !r_sel_map with
             Some index -> if index < n then index else (-1)
           | None -> (-1)
         in

         let div_show = Dom_html.createDiv document in
         Dom.appendChild elem div_show;
         div_show##.className := Js.string "state-compo";
         add_title div_show title_opt;

         let div_desc = Dom_html.createDiv document in
         Dom.appendChild div_show div_desc;
         div_desc##.className := Js.string "show-compound";

         let div_list = Dom_html.createDiv document in
         Dom.appendChild div_desc div_list;
         div_list##.className := Js.string "v-container vscroll";
         for i=0 to n-1 do
           let d = Dom_html.createDiv document in
           d##.className := Js.string "menu-ent-div-hier";
           let text = sprintf "%d. %s" i (show pos state i) in
           d##.textContent := Js.some (Js.string text);
           (if i = sel_index then
              (d##.style##.backgroundColor := Js.string Color.background_selected;
               d##.style##.color := Js.string Color.text_selected));
           d##.onclick := Dom_html.handler (onclick base_opt state spec0 skel0 id i);
           Dom.appendChild div_list d
         done;
         add_sep div_desc;
         cons div_desc sub_spec sub_skel (pos @ [sel_index])
      | _ -> ()
    in
    element##.innerHTML := Js.string "";
    cons element spec0 skel0 [];
    add_sep element
  in

  let clear () =
    element##.innerHTML := Js.string "";
  in

  let update_view ?(base_opt = None) state =
    let spec = config.state_spec state in
    let spec_skel = extract_spec_skeleton spec in
    (if not (SpecSkeleton.equal spec_skel !r_spec_skel) then
       (r_sel_map := IntMap.empty;
        r_spec_skel := spec_skel;
        match SpecMap.find_opt spec_skel !r_spec_skel_map with
          Some sel_map -> r_sel_map := sel_map
        | None -> ()));
    match base_opt with
      Some _base ->
       let base_spec = config.state_spec state in
       let base_spec_skel = extract_spec_skeleton base_spec in
       if SpecSkeleton.equal base_spec_skel spec_skel then
         construct ~base_opt state spec spec_skel
       else
         construct state spec spec_skel
    | None ->
       construct state spec spec_skel
  in


  let update_current_state ui_state =
    UiState.state_case ui_state
      (fun _trans_index_pointed_opt
           _hist_index_pointed_opt _hist_index hist_ent ->
        update_view hist_ent.state)
      (fun _trans_index_pointed_opt ->
        update_view ui_state.model.current_state)
  in

  let update_target_state ui_state =
    UiState.state_case ui_state
      (fun trans_index_pointed_opt
           _hist_index_pointed_opt _hist_index hist_ent ->
        match hist_ent.trans_kind with
          EventTrans (_label, trans_index_hist) ->
           let trans_index =
             match trans_index_pointed_opt with
               Some i -> i  | None -> trans_index_hist
           in
           let transitions = config.transf hist_ent.state hist_ent.time in
           let n = List.length transitions in
           if 0 <= trans_index && trans_index < n then
             let trans =
               Util.list_nth __FUNCTION__ transitions trans_index
             in
             (match trans.time_constraint with
                Enabled _ -> update_view ~base_opt:(Some hist_ent.state) trans.target
              | Disabled _ -> assert false)
           else
             clear ()
        | TimeTrans _delta ->
           clear ())
      (fun trans_index_pointed_opt ->
        match trans_index_pointed_opt with
          Some trans_index_pointed ->
           let transitions =
             config.transf
               ui_state.model.current_state
               ui_state.model.current_time
           in
           let n = List.length transitions in
           if 0 <= trans_index_pointed && trans_index_pointed < n then
             let trans =
               List.nth transitions trans_index_pointed
             in
             (match trans.time_constraint with
                Enabled _ ->
                 let current_state = ui_state.model.current_state in
                 update_view ~base_opt:(Some current_state) trans.target
              | Disabled _ -> assert false)
           else
             (Printf.printf "%s: index out of range\n" __FUNCTION__;
              clear ())
        | None ->
           clear ())
  in

  let on_mousemove _event =
    inject ech Unpoint;
    Js._false
  in

  element##.onmousemove := Dom_html.handler on_mousemove;

  let rec process () =
    let event_list = [
        recvEvt dch handle;
        recvEvt ech handle_event;
      ]
    in
    Util.select_que event_list uque

  and handle (Update (_hint, ui_state)) =
    (match state_selection with
       CurrentState -> update_current_state ui_state
     | TargetState -> update_target_state ui_state);
    process ()

  and handle_event msg =
    request msg; process ()

  and request msg =
    Queue.add (sendEvt uch msg uque_drop) uque;

  and uque_drop () = let _ = Queue.take uque in process ()

  in

  process ()
