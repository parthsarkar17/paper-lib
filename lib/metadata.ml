module StringSet = Set.Make (String)

type t = { paper_filename : string; keywords : StringSet.t }

let build (paper_filename : string) (keywords : string list) : t =
  let keyword_set = StringSet.of_list keywords in
  { paper_filename; keywords = keyword_set }

let query (queries : string list) (metadata : t) : string list =
  List.filter (fun query -> StringSet.mem query metadata.keywords) queries

let filename (metadata : t) : string = metadata.paper_filename

let from_json (json : Yojson.Basic.t) : t =
  let paper_filename =
    json
    |> Yojson.Basic.Util.member "paper_filename"
    |> Yojson.Basic.Util.to_string
  in
  let keywords =
    json
    |> Yojson.Basic.Util.member "keywords"
    |> Yojson.Basic.Util.convert_each Yojson.Basic.Util.to_string
  in
  build paper_filename keywords
