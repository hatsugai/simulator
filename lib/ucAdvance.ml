open Csp
open Model

let init
      (html_elem_id : string)
      (uch : ('label, 'state)up_message chan)
      (dch : ('label, 'state) down_message chan)
  =
  UcButton.init html_elem_id uch dch Advance
    (function
       Update (_hint, ui_state) ->
        (match ui_state.time_spec with
           TS_None -> false
         | TS_Absolute time -> ui_state.model.current_time < time
         | TS_Relative _ -> true))
