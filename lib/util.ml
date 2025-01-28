open Printf

module IntMap = Map.Make (struct type t = int let compare = compare end)

let rec drop xs i =
  if i=0 then
    xs
  else
    match xs with
      [] -> assert false
    | _::xs' -> drop xs' (i-1)

let cs_to_str (cs : char list) : string =
  let buf = Buffer.create 0 in
  List.iter (Buffer.add_char buf) cs;
  Buffer.contents buf

let split_lines (text : string) : string list =
  let n = String.length text in
  let rec loop acc cs i =
    if i=n then
      let acc = if cs=[] then acc else cs::acc in
      List.rev (List.map (fun cs -> (cs_to_str (List.rev cs))) acc)
    else
      let c = text.[i] in
      if c = '\n' then
        loop (cs::acc) [] (i+1)
      else
        loop acc (c::cs) (i+1)
  in
  loop [] [] 0

let select_que event_list que =
  let event_list =
    if Queue.is_empty que then
      event_list
    else
      (Queue.top que)::event_list
  in
  Csp.select event_list

let is_digit (c : char) =
  c >= '0' && c <= '9'

let list_nth fun_name xs i =
  let n = List.length xs in
  if 0 <= i && i < n then
    List.nth xs i
  else
    failwith
      (Printf.sprintf "list_nth: index out of range: %s" fun_name)

let rec update xs i a =
  match xs with
    [] -> Error.f "%s" __FUNCTION__
  | x::xs' ->
     if i=0 then
       a::xs'
     else
       x::(update xs' (i-1) a)

let rec iter3 f xs ys zs =
  match xs, ys, zs with
    x::xs', y::ys', z::zs' ->
    f x y z; iter3 f xs' ys' zs'
  | _ -> ()

let show_list show sep xs =
  match xs with
    [] -> ""
  | [x] -> show x
  | x::xs' ->
     List.fold_left
       (fun s x -> sprintf "%s%s%s" s sep (show x))
       (show x) xs'

let show_float x = sprintf "%f" x

let max_list xs = List.fold_left max min_int xs
let min_list xs = List.fold_left min max_int xs

let get_elem (id : string) =
  let open Js_of_ocaml in
  Js.Opt.get (Dom_html.document##getElementById (Js.string id))
    (fun () ->
      Error.f "DOM element not found: %s" id)

let hide_elem (id : string) =
  let open Js_of_ocaml in
  let elem = get_elem id in
  elem##.style##.display := Js.string "none"
