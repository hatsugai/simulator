open Simulator.Config

module Event =
  struct
    type t = Tau | A | B | C | Ret
    [@@deriving show { with_path=false }, eq, ord]
  end

module State = struct
  type t = S0 of int | S1
  [@@deriving show { with_path=false }, eq, ord]

  let initial_state = S0 0

  let transf state time =
    let open Event in
    match state with
      S0 time_in ->
       [(Enabled None, A, S1)]
       @ (if time = time_in + 10 then [(Enabled None, Tau, S1)]
          else if time < time_in + 10 then [(Disabled (time_in + 10), Tau, S1)]
          else [])
       @ (if time <= time_in + 3 then [(Enabled (Some (time_in + 3)), B, S1)] else [])
       @ (if time < time_in + 3 then
            [(Disabled (time_in + 3), C, S1)]
          else if time <= time_in + 7 then
            [(Enabled (Some (time_in + 7)), C, S1)]
          else [])
    | S1 -> [(Enabled None, Ret, S0 time)]

  let state_time_constraint state _time =
    match state with
      S0 time_in -> Some (time_in + 10)
    | S1 -> None
end

let conv_transf transf state time =
  List.map (fun (time_constraint, label, target) ->
      { time_constraint; label; target; party = Alone 0; })
    (transf state time)

let state_spec (_state : State.t) =
  Plain { title_opt = None; show = (fun _pos state -> State.show state) }

let () =
  let config = {
      title = "Example: Time Constraint";
      state = State.initial_state;
      history = [];
      component_name_list = [];
      transf = conv_transf State.transf;
      state_time_constraint_opt = Some State.state_time_constraint;
      compare_label_opt = Some Event.compare;
      compare_state_opt = Some State.compare;
      show_label = Event.show;
      state_spec;
      additional_advance_buttons = [1; 2; 3; 4; 5; 10; 50; 100];
      sequence_diagram_ppi_opt = None;
      sequence_diagram_inter_component_opt = None;
    }
  in
  Simulator.Init.f config
