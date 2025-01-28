open Csp
open Model

let init
      (html_elem_id : string)
      (uch : ('label, 'state)up_message chan)
      (dch : ('label, 'state) down_message chan)
  =
  UcButton.init html_elem_id uch dch Clone (fun _ -> true)
