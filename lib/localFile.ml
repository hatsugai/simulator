open Js_of_ocaml

let save title data =
  let bytes = Marshal.to_bytes (title, data) [Marshal.Compat_32] in
  let data = Typed_array.Bytes.to_uint8Array bytes in
  let blob =
    File.blob_from_any [`arrayBuffer (data :> Typed_array.arrayBuffer Js.t)]
  in
  let url = Dom_html.window##.URL##createObjectURL blob in
  let anchor = Dom_html.createA Dom_html.document in
  let filename = Printf.sprintf "%s.save" title in
  anchor##.href := url;
  anchor##.download := Js.string filename;
  anchor##click;
  Dom_html.window##.URL##revokeObjectURL url

let setup_download (action : bytes -> unit) =
  let element : Dom_html.element Js.t = Util.get_elem "file" in

  let input : Dom_html.inputElement Js.t =
    match Js.Opt.to_option (Dom_html.CoerceTo.input element) with
      Some input -> input
    | None -> Error.f "cannot get button: file"
  in

  let on_load_file
        (file_reader : File.fileReader Js.t)
        (_event : File.fileReader File.progressEvent Js.t) =
    let array_buf_opt = File.CoerceTo.arrayBuffer file_reader##.result in
    match Js.Opt.to_option array_buf_opt with
    | Some array_buf ->
       let bytes : bytes = Typed_array.Bytes.of_arrayBuffer array_buf in
       action bytes;
       Js._true
    | None -> Js._true
  in

  let on_file_selected input _ =
    let file_list : File.fileList Js.t = input##.files in
    if file_list##.length > 0 then
      let file : File.file Js.t Js.opt = file_list##item 0 in
      begin
        match Js.Opt.to_option file with
        | Some (file : File.file Js.t) ->
           let file_reader : File.fileReader Js.t =
             new%js File.fileReader
           in
           file_reader##.onload :=
             Dom.handler (on_load_file file_reader);
           file_reader##readAsArrayBuffer file
        | None -> ()
      end;
      Js._true
    else
      Js._true
  in
  input##.onchange :=
    Dom_html.handler (on_file_selected input)

let open_file_dialog () =
  let element : Dom_html.element Js.t = Util.get_elem "file" in
  let input : Dom_html.inputElement Js.t =
    match Js.Opt.to_option (Dom_html.CoerceTo.input element) with
      Some input -> input
    | None -> Error.f "cannot get button: file"
  in
  input##click
