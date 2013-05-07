
type t =
  | Int of int

let type_of_literal lit =
  begin match lit with
    | Int(_) -> Type.Con(Ident.intern "int")
  end
