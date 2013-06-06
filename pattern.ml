open Printf

type t =
  | Con of Literal.t
  | Var of Names.val_name
  | Variant of (Names.ctor * (t) list)
  | Tuple of (t) list
  | Record of ((Names.val_path * (t) option)) list
  | Or of (t * t)

let rec show = begin fun pat ->
  begin match pat with
    | (Con(lit)) ->
      ((sprintf "Con(%s)") (Literal.show lit))
    | (Var(name)) ->
      ((sprintf "Var(%s)") (Names.show_val_name name))
    | (Variant(ctor, pat_list)) ->
      ((sprintf "Variant(%s)") (Names.show_ctor ctor))
    | (Tuple(pat_list)) ->
      (sprintf "Tuple()")
    | (Record(field_list)) ->
      (sprintf "Record()")
    | (Or(lhs, rhs)) ->
      (((sprintf "Or(%s,%s)") (show lhs)) (show rhs))
  end
end

