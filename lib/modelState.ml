open Config

type ('label, 'state) model = {
    current_state : 'state;
    current_time : int;
    history : ('label, 'state) history_entry list;
}

let get_model (config : ('label, 'state) simulator_config) =
  let current_time =
    match config.history with
      [] -> 0
    | hist_ent::_ ->
       (match hist_ent.trans_kind with
          EventTrans _ -> hist_ent.time
        | TimeTrans delta -> hist_ent.time + delta)
  in
  {
    current_state = config.state;
    current_time;
    history = [];
  }


let reset config =
  {
    current_state = config.state;
    current_time = 0;
    history = []
  }

let select config (m : ('label, 'state) model) (trans_index : int) =
  let transitions = config.transf m.current_state m.current_time in
  let trans =
    Util.list_nth __FUNCTION__ transitions trans_index
  in
  match trans.time_constraint with
    Enabled _time_limit_opt ->
     {
       m with
       history =
         {
           trans_kind = EventTrans (trans.label, trans_index);
           state = m.current_state;
           time = m.current_time;
           party = Some trans.party;
           memo = "";
         }
         :: m.history;
       current_state = trans.target;
     }
  | Disabled _time_limit ->
     assert false

let go_back_to_history_entry (m : ('label, 'state) model) hist_index =
  match Util.drop m.history hist_index with
    [] -> assert false
  | hist_ent::history' ->
     {
       current_state = hist_ent.state;
       current_time = hist_ent.time;
       history = history';
     }

let advance_time (m : ('label, 'state) model) time_delta =
  { m with
    history =
      {
        trans_kind = TimeTrans time_delta;
        state = m.current_state;
        time = m.current_time;
        party = None;
        memo = "";
      }
      :: m.history;
    current_time = m.current_time + time_delta;
  }

let update_memo (m : ('label, 'state) model) hist_index memo =
  let hist_ent = Util.list_nth __FUNCTION__ m.history hist_index in
  let hist_ent = { hist_ent with memo } in
  let history = Util.update m.history hist_index hist_ent in
  { m with history }
