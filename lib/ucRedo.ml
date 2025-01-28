open Csp
open Model

let init
      (html_elem_id : string)
      (uch : ('label, 'state) up_message chan)
      (dch : ('label, 'state) down_message chan)
  =
  UcButton.init html_elem_id uch dch Redo
    (function Update (_, ui_state) -> UiState.redoable ui_state)
