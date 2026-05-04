module IntMap = Map.Make (Int)

type t = { filename : string; bytes : Bytes.t }

let compress (filepath : string) : t =
  let file_handle = In_channel.open_bin filepath in
  let length = file_handle |> In_channel.length |> Int64.to_int in
  let bytes_buf = Bytes.create length in
  match In_channel.really_input file_handle bytes_buf 0 length with
  | None -> failwith "Couldn't read file bytes into buffer"
  | Some () ->
      let lzw_map : int option array array = Array.make_matrix 8129 256 None in
      { filename = ""; bytes = Bytes.create 10 }
