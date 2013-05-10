
type t =
  | Unit
  | Int of int
  | Bool of bool

let show lit =
  begin match lit with
    | Unit -> "()"
    | Int(n) -> string_of_int n
    | Bool(b) -> string_of_bool b
  end
