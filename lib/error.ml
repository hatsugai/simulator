exception Error of string

let f fmt =
  Printf.ksprintf (fun s -> raise (Error s)) fmt
