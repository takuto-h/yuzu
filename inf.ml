open Printf

type t = {
  mods : ((Names.mod_name * Module.t)) list;
  asp : ((Names.val_name * Scheme.t)) list;
  let_level : int;
}

let rec create = begin fun (()(_)) ->
  {
    mods = [];
    asp = [];
    let_level = 0;
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

let rec find_asp = begin fun inf ->
  begin fun path ->
    begin match path with
      | (([](_)), name) ->
        ((List.assoc name) inf.asp)
      | ((( :: )(mod_name, mod_path)), name) ->
        begin let modl = ((List.assoc mod_name) inf.mods) in
        (((Module.find_asp modl) mod_path) name)
        end
    end
  end
end

let rec instantiate = begin fun let_level ->
  begin fun {Scheme.gen_num;Scheme.body;} ->
    begin let type_vars = ((Array.init gen_num) begin fun _ ->
      (Type.make_var let_level)
    end) in
    ((Type.map (Array.get type_vars)) body)
    end
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

let rec infer = begin fun inf ->
  begin fun expr ->
    begin match expr.Expr.raw with
      | (Expr.Con(lit)) ->
        (infer_literal lit)
      | (Expr.Var(path)) ->
        begin try
          ((instantiate inf.let_level) ((find_asp inf) path))
        with

          | (Not_found(_)) ->
            (failwith ((unbound_variable expr.Expr.pos) path))
        end
      | (Expr.App(fun_expr, arg_expr)) ->
        begin let fun_type = ((infer inf) fun_expr) in
        begin let arg_type = ((infer inf) arg_expr) in
        begin let ret_type = (Type.make_var inf.let_level) in
        begin let rec occurs_check_error = begin fun (()(_)) ->
          (failwith "")
        end in
        begin let rec unification_error = begin fun t1 ->
          begin fun t2 ->
            (failwith "")
          end
        end in
        begin
        ((((Type.unify occurs_check_error) unification_error) fun_type) (Type.Fun (arg_type, ret_type)));
        ret_type
        end
        end
        end
        end
        end
        end
    end
  end
end

let mod_B = ((Module.make []) (( :: ) (((Names.Id ("b1")), (Scheme.mono char_type)), (( :: ) (((Names.Id ("b2")), (Scheme.mono int_type)), [])))))

let mod_A = ((Module.make (( :: ) (("B", mod_B), []))) (( :: ) (((Names.Id ("a1")), (Scheme.mono int_type)), (( :: ) (((Names.Id ("a2")), (Scheme.mono string_type)), [])))))

let inf = {
  mods = (( :: ) (("A", mod_A), []));
  asp = (( :: ) (((Names.Id ("ans")), (Scheme.mono int_type)), []));
  let_level = 0;
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

