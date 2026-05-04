module IntMap = Map.Make (Int)

type t = { compressed : bool; bytes : Bytes.t }


