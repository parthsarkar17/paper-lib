open Paper_lib

module Exec_context = struct
  type t =
    | Query of { queries : string list }
    | AddEntry of { paper_filename : string; keywords : string list }
    | Help of { error_msg : string }

  let parse_argv (args : string array) : t =
    let error_msg =
      "Bad Arguments: (<'query'> <keyword+> | <'add'> <paper_filename> <keyword+>)"
    in
    match Array.to_list args with
    | _ :: "query" :: queries ->
        if List.length queries > 0 then Query { queries }
        else Help { error_msg }
    | _ :: "add" :: paper_filename :: keywords ->
        if List.length keywords > 0 then AddEntry { paper_filename; keywords }
        else Help { error_msg }
    | _ -> Help { error_msg }

  let to_string : t -> string = function
    | Query { queries } ->
        let queries_string = String.concat ", " queries in
        "Query: " ^ queries_string
    | AddEntry { paper_filename; keywords } ->
        let keywords_string = String.concat ", " keywords in
        "AddEntry: add filename '" ^ paper_filename ^ "' with keywords "
        ^ keywords_string
    | Help { error_msg } -> "Help: " ^ error_msg
end

let parse_metadata_json (metadata_fp : string) : Metadata.t list =
  metadata_fp |> Yojson.Basic.from_file
  |> Yojson.Basic.Util.convert_each Metadata.from_json

let query_handler (metadata_objs : Metadata.t list) (queries : string list) :
    unit =
  metadata_objs
  |> List.filter_map (fun metadata_obj ->
      match Metadata.query queries metadata_obj with
      | [] -> None
      | matching_queries ->
          let matching_filename = Metadata.filename metadata_obj in
          Some (matching_filename, matching_queries))
  |> List.iter (fun (filename, keywords) ->
      let match_msg =
        "File '" ^ filename ^ "' matches keywords: \n    "
        ^ String.concat "\n    " keywords
      in
      print_endline match_msg)

(* let parse_argv *)

let () =
  Sys.argv |> Exec_context.parse_argv |> Exec_context.to_string |> print_endline
