open Printf

type t = {
  mods : ((Names.mod_name * Module.t)) list;
  asp : ((Names.val_name * Type.t)) list;
}

let rec create = begin fun (()(_)) ->
  {
    mods = [];
    asp = [];
  }
end

let int_type = (Type.Con ([], "int"))

let string_type = (Type.Con ([], "string"))

let char_type = (Type.Con ([], "char"))

let rec unbound_variable = begin fun pos ->
  begin fun path ->
    (((sprintf "%s: error: unbound variable: %s\n") (Pos.show pos)) (Names.show_val_path path))
  end
end

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

let rec infer_var = begin fun inf ->
  begin fun pos ->
    begin fun path ->
      begin match path with
        | (([](_)), name) ->
          begin try
            ((List.assoc name) inf.asp)
          with

            | (Not_found(_)) ->
              (failwith ((unbound_variable pos) path))
          end
        | ((( :: )(mod_name, mod_path)), name) ->
          begin try
            begin let modl = ((List.assoc mod_name) inf.mods) in
            (((Module.find_asp modl) mod_path) name)
            end
          with

            | (Not_found(_)) ->
              (failwith ((unbound_variable pos) path))
          end
      end
    end
  end
end

let rec infer = begin fun inf ->
  begin fun expr ->
    begin match expr.Expr.raw with
      | (Expr.Con(lit)) ->
        (infer_literal lit)
      | (Expr.Var(path)) ->
        (((infer_var inf) expr.Expr.pos) path)
    end
  end
end

