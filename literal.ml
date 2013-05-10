
type t =
  | Int of int
  | Bool of bool

let type_of_literal lit =
  begin match lit with
    | Int(_) -> Type.Con(Ident.intern "Int")
    | Bool(_) -> Type.Con(Ident.intern "Bool")
  end

let show lit =
  begin match lit with
    | Int(n) -> string_of_int n
    | Bool(b) -> string_of_bool b
  end
