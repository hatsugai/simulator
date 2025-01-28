type time_constraint = Enabled of int option | Disabled of int

type party =
  Alone of int
| Communication of int * int
| Multiparty of int list
[@@deriving show { with_path=false }]

type ('label, 'state) transition = {
    time_constraint : time_constraint;
    party : party;
    label : 'label;
    target : 'state;
}

type 'label trans_kind =
  EventTrans of 'label * int
| TimeTrans of int

type ('label, 'state) history_entry = {
    trans_kind : 'label trans_kind;
    state : 'state;
    time : int;
    party : party option;
    memo : string;
}

type pos = int list

type 'state spec =
  Plain of {
      title_opt : string option;
      show : pos -> 'state -> string;
    }
| Compound of {
    title_opt : string option;
    spec_list : 'state spec list
  }
| List of {
    title_opt : string option;
    length : pos -> 'state -> int;
    show : pos -> 'state -> int -> string;
  }
| Hierarchy of {
    title_opt : string option;
    sub_spec : 'state spec;
    length : pos -> 'state -> int;
    show : pos -> 'state -> int -> string;
  }

type ('label, 'state) simulator_config = {
    title : string;
    state : 'state;
    history : ('label, 'state) history_entry list;
    component_name_list : string list;
    transf : 'state -> int -> ('label, 'state) transition list;
    state_time_constraint_opt : ('state -> int -> int option) option;
    compare_label_opt : ('label -> 'label -> int) option;
    compare_state_opt : ('state -> 'state -> int) option;
    show_label : 'label -> string;
    state_spec : 'state -> 'state spec;
    additional_advance_buttons : int list;
    sequence_diagram_ppi_opt : float option;
    sequence_diagram_inter_component_opt : float option;
}
