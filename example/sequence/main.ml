open Simulator.Config

module Event =
  struct
    type t = A | B | C | AB | AC | BA | BC | CB | ABC
    [@@deriving show { with_path=false }, eq, ord]
  end

module State = struct
  type t = S
  [@@deriving show { with_path=false }, eq, ord]

  let initial_state = S

  let transf state _time =
    let open Event in
    match state with
      S ->
      [
        { time_constraint = Enabled None;
          label = A;
          party = Alone 0;
          target = S; };
        { time_constraint = Enabled None;
          label = B;
          party = Alone 1;
          target = S; };
        { time_constraint = Enabled None;
          label = C;
          party = Alone 2;
          target = S; };
        { time_constraint = Enabled None;
          label = AB;
          party = Communication (0, 1);
          target = S; };
        { time_constraint = Enabled None;
          label = AC;
          party = Multiparty [0; 2];
          target = S; };
        { time_constraint = Enabled None;
          label = BA;
          party = Communication (1, 0);
          target = S; };
        { time_constraint = Enabled None;
          label = BC;
          party = Communication (1, 2);
          target = S; };
        { time_constraint = Enabled None;
          label = CB;
          party = Communication (2, 1);
          target = S; };
        { time_constraint = Enabled None;
          label = ABC;
          party = Multiparty [0; 1; 2];
          target = S; };
      ]
end

let state_spec (_state : State.t) =
  Plain { title_opt = None; show = (fun _pos state -> State.show state) }

let () =
  let config = {
      title = "Sequence Diagram Example";
      state = State.initial_state;
      history = [];
      component_name_list = ["A"; "B"; "C"];
      transf = State.transf;
      state_time_constraint_opt = None;
      compare_label_opt = None;
      compare_state_opt = None;
      show_label = Event.show;
      state_spec;
      additional_advance_buttons = [];
      sequence_diagram_ppi_opt = None;
      sequence_diagram_inter_component_opt = None;
    }
  in
  Simulator.Init.f config
