open Printf

type t =
  | Con of Literal.t
  | Var of Names.val_name
  | Variant of (Names.ctor * (t) list)

let rec show = begin fun pat ->
  begin match pat with
    | Con(lit) ->
      ((sprintf "Con(%s)") (Literal.show lit))
    | Var(name) ->
      ((sprintf "Var(%s)") (Names.show_val_name name))
    | Variant(ctor, pat_list) ->
      ((sprintf "Variant(%s)") (Names.show_ctor ctor))
  end
end

