open Printf

type t = {
  mods : ((Names.mod_name * Module.t)) list;
  asp : ((Names.val_name * Scheme.t)) list;
  let_level : int;
}

let rec create = begin fun (() _) ->
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

let rec invalid_application = begin fun pos ->
  begin fun fun_type ->
    begin fun arg_type ->
      begin fun t1 ->
        begin fun t2 ->
          begin let shower = (Type.create_shower 0) in
          ((((((sprintf "%s: error: invalid application\n%s%s%s%s") (Pos.show pos)) ((sprintf "function type: %s\n") ((Type.show shower) fun_type))) ((sprintf "argument type: %s\n") ((Type.show shower) arg_type))) (((Type.show_origin shower) "function type") t1)) (((Type.show_origin shower) "argument type") t2))
          end
        end
      end
    end
  end
end

let rec find_asp = begin fun inf ->
  begin fun path ->
    begin match path with
      | (([] _), name) ->
        ((List.assoc name) inf.asp)
      | ((( :: ) (mod_name, mod_path)), name) ->
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
    | (Literal.Int (_)) ->
      int_type
    | (Literal.String (_)) ->
      string_type
    | (Literal.Char (_)) ->
      char_type
  end
end

let rec infer_pattern = begin fun inf ->
  begin fun pat ->
    begin match pat with
      | (Pattern.Con (lit)) ->
        (inf, ((Type.at None) (infer_literal lit)))
      | (Pattern.Var (name)) ->
        begin let t = (Type.make_var inf.let_level) in
        begin let inf = {
          inf with
          asp = (( :: ) ((name, (Scheme.mono t)), inf.asp));
        } in
        (inf, t)
        end
        end
      | (Pattern.Tuple (pats)) ->
        begin let (inf, ts) = (((YzList.fold_right pats) (inf, [])) begin fun elem ->
          begin fun (inf, ts) ->
            begin let (inf, t) = ((infer_pattern inf) elem) in
            (inf, (( :: ) (t, ts)))
            end
          end
        end) in
        (inf, ((Type.at None) (Type.Tuple (ts))))
        end
    end
  end
end

let rec infer_expr = begin fun inf ->
  begin fun expr ->
    begin match expr.Expr.raw with
      | (Expr.Con (lit)) ->
        ((Type.at (Some (expr.Expr.pos))) (infer_literal lit))
      | (Expr.Var (path)) ->
        begin try
          ((instantiate inf.let_level) ((find_asp inf) path))
        with

          | (Not_found _) ->
            (failwith ((unbound_variable expr.Expr.pos) path))
        end
      | (Expr.App (fun_expr, arg_expr)) ->
        begin let fun_type = ((infer_expr inf) fun_expr) in
        begin let arg_type = ((infer_expr inf) arg_expr) in
        begin let ret_type = (Type.make_var inf.let_level) in
        begin
        begin try
          ((Type.unify fun_type) ((Type.at None) (Type.Fun (arg_type, ret_type))))
        with

          | (Type.Unification_error (t1, t2)) ->
            (failwith (((((invalid_application expr.Expr.pos) fun_type) arg_type) t1) t2))
        end;
        ret_type
        end
        end
        end
        end
      | (Expr.Abs (pat, body_expr)) ->
        begin let (inf, pat_type) = ((infer_pattern inf) pat) in
        begin let body_type = ((infer_expr inf) body_expr) in
        ((Type.at (Some (expr.Expr.pos))) (Type.Fun (pat_type, body_type)))
        end
        end
    end
  end
end

let rec infer_top = begin fun inf ->
  begin fun top ->
    begin match top.Top.raw with
      | (Top.Expr (expr)) ->
        (inf, ((infer_expr inf) expr))
    end
  end
end

let pos = ((((Pos.make "<assertion>") 1) 0) 0)

let mod_B = ((Module.make []) (( :: ) (((Names.Id ("b1")), (Scheme.mono ((Type.at (Some (pos))) char_type))), (( :: ) (((Names.Id ("b2")), (Scheme.mono ((Type.at (Some (pos))) int_type))), [])))))

let mod_A = ((Module.make (( :: ) (("B", mod_B), []))) (( :: ) (((Names.Id ("a1")), (Scheme.mono ((Type.at (Some (pos))) int_type))), (( :: ) (((Names.Id ("a2")), (Scheme.mono ((Type.at (Some (pos))) string_type))), [])))))

let inf = {
  mods = (( :: ) (("A", mod_A), []));
  asp = (( :: ) (((Names.Id ("ans")), (Scheme.mono ((Type.at (Some (pos))) int_type))), []));
  let_level = 0;
}

let shower = (Type.create_shower 0)

let int_expr = ((Expr.at pos) (Expr.Con ((Literal.Int (123)))))

let string_expr = ((Expr.at pos) (Expr.Con ((Literal.String ("abc")))))

let char_expr = ((Expr.at pos) (Expr.Con ((Literal.Char ("x")))))

let () = (assert ((( = ) ((Type.show shower) ((infer_expr inf) int_expr))) "int"))

let () = (assert ((( = ) ((Type.show shower) ((infer_expr inf) string_expr))) "string"))

let () = (assert ((( = ) ((Type.show shower) ((infer_expr inf) char_expr))) "char"))

let ans = ((Expr.at pos) (Expr.Var ([], (Names.Id ("ans")))))

let _A_a2 = ((Expr.at pos) (Expr.Var ((( :: ) ("A", [])), (Names.Id ("a2")))))

let _A_B_b1 = ((Expr.at pos) (Expr.Var ((( :: ) ("A", (( :: ) ("B", [])))), (Names.Id ("b1")))))

let () = (assert ((( = ) ((Type.show shower) ((infer_expr inf) ans))) "int"))

let () = (assert ((( = ) ((Type.show shower) ((infer_expr inf) _A_a2))) "string"))

let () = (assert ((( = ) ((Type.show shower) ((infer_expr inf) _A_B_b1))) "char"))

let app_expr = ((Expr.at pos) (Expr.App (int_expr, string_expr)))

let () = begin try
  (ignore ((infer_expr inf) app_expr))
with

  | (Failure (got)) ->
    begin let req = (((((sprintf "%s%s%s%s") "<assertion>:1:0: error: invalid application\n") "function type: int\n") "argument type: string\n") "<assertion>:1:0: 'int' of function type\n") in
    (assert ((( = ) got) req))
    end
end

