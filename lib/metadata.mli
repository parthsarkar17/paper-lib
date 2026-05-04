type t

val build : string -> string list -> t

val query : string list -> t -> string list

val filename : t -> string

val from_json : Yojson.Basic.t -> t