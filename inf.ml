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

let mod_B = ((Module.make []) (( :: ) (((Names.Id ("b1")), char_type), (( :: ) (((Names.Id ("b2")), int_type), [])))))

let mod_A = ((Module.make (( :: ) (("B", mod_B), []))) (( :: ) (((Names.Id ("a1")), int_type), (( :: ) (((Names.Id ("a2")), string_type), [])))))

let inf = {
  mods = (( :: ) (("A", mod_A), []));
  asp = (( :: ) (((Names.Id ("ans")), int_type), []));
}

let pos = ((((Pos.make "<assertion>") 1) 0) 0)

let int_expr = ((Expr.at pos) (Expr.Con ((Literal.Int (123)))))

let string_expr = ((Expr.at pos) (Expr.Con ((Literal.String ("abc")))))

let char_expr = ((Expr.at pos) (Expr.Con ((Literal.Char ("x")))))

let () = (assert ((( = ) ((infer inf) int_expr)) int_type))

let () = (assert ((( = ) ((infer inf) string_expr)) string_type))

let () = (assert ((( = ) ((infer inf) char_expr)) char_type))

let ans = ((Expr.at pos) (Expr.Var ([], (Names.Id ("ans")))))

let _A_a2 = ((Expr.at pos) (Expr.Var ((( :: ) ("A", [])), (Names.Id ("a2")))))

let _A_B_b1 = ((Expr.at pos) (Expr.Var ((( :: ) ("A", (( :: ) ("B", [])))), (Names.Id ("b1")))))

let () = (assert ((( = ) ((infer inf) ans)) int_type))

let () = (assert ((( = ) ((infer inf) _A_a2)) string_type))

let () = (assert ((( = ) ((infer inf) _A_B_b1)) char_type))

