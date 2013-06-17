type t = {
  dummy : unit;
}

let rec create = begin fun (()(_)) ->
  {
    dummy = ();
  }
end

let int_type = (Type.Con ([], "int"))

let string_type = (Type.Con ([], "string"))

let char_type = (Type.Con ([], "char"))

let rec infer_literal = begin fun lit ->
  begin match lit with
    | (Literal.Int(_)) ->
      int_type
    | (Literal.String(_)) ->
      string_type
    | (Literal.Char(_)) ->
      char_type
  end
end

let rec infer = begin fun inf ->
  begin fun expr ->
    begin match expr with
      | (Expr.Con(lit)) ->
        (infer_literal lit)
    end
  end
end

