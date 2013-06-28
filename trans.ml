open Printf

type t = {
  basic_offset : int;
  indent_level : int;
}

let rec create = begin fun basic_offset ->
  {
    basic_offset = basic_offset;
    indent_level = 0;
  }
end

let initial_buffer_size = 256

let rec incr_indent_level = begin fun trans ->
  {
    trans with
    indent_level = ((( + ) trans.indent_level) 1);
  }
end

let rec indent = begin fun {basic_offset;indent_level;} ->
  begin fun str ->
    begin let offset = ((( * ) basic_offset) indent_level) in
    (((sprintf "%s%s") ((String.make offset) ' ')) str)
    end
  end
end

let rec translate_literal = begin fun lit ->
  begin match lit with
    | (Literal.Unit _) ->
      "()"
    | (Literal.Int (n)) ->
      ((sprintf "%d") n)
    | (Literal.String (str)) ->
      ((sprintf "\"%s\"") str)
    | (Literal.Char (str)) ->
      ((sprintf "'%s'") str)
    | (Literal.Bool (bool)) ->
      ((sprintf "%B") bool)
  end
end

let rec translate_pattern = begin fun pat ->
  begin match pat with
    | (Pattern.Con (lit)) ->
      (translate_literal lit)
    | (Pattern.Var (name)) ->
      (Names.show_val_name name)
    | (Pattern.Variant (ctor, pat)) ->
      begin let str_ctor = (Names.show_ctor ctor) in
      begin let str_pat = (translate_pattern pat) in
      (((sprintf "(%s %s)") str_ctor) str_pat)
      end
      end
    | (Pattern.Tuple ((( :: ) (pat, pats)))) ->
      begin let str_pat_list = (((YzList.fold_left (translate_pattern pat)) pats) begin fun acc ->
        begin fun elem ->
          (((sprintf "%s, %s") acc) (translate_pattern elem))
        end
      end) in
      ((sprintf "(%s)") str_pat_list)
      end
    | (Pattern.Tuple (([] _))) ->
      (assert false)
    | (Pattern.Record (fields)) ->
      begin let str_fields = (((YzList.fold_left "") fields) begin fun acc ->
        begin fun elem ->
          (((sprintf "%s%s;") acc) (translate_field_pattern elem))
        end
      end) in
      ((sprintf "{%s}") str_fields)
      end
    | (Pattern.Or (lhs, rhs)) ->
      begin let str_lhs = (translate_pattern lhs) in
      begin let str_rhs = (translate_pattern rhs) in
      (((sprintf "(%s | %s)") str_lhs) str_rhs)
      end
      end
    | (Pattern.As (pat, name)) ->
      begin let str_pat = (translate_pattern pat) in
      begin let str_name = (Names.show_val_name name) in
      (((sprintf "(%s as %s)") str_pat) str_name)
      end
      end
  end
end

and translate_field_pattern = begin fun field_pat ->
  begin match field_pat with
    | (path, (None _)) ->
      ((sprintf "%s") (Names.show_val_path path))
    | (path, (Some (pat))) ->
      (((sprintf "%s=%s") (Names.show_val_path path)) (translate_pattern pat))
  end
end

let rec translate_expr = begin fun trans ->
  begin fun expr ->
    begin match expr.Expr.raw with
      | (Expr.Con (lit)) ->
        (translate_literal lit)
      | (Expr.Var (path)) ->
        (Names.show_val_path path)
      | (Expr.Abs (param_pat, body_expr)) ->
        begin let str_param = (translate_pattern param_pat) in
        begin let trans_body = (incr_indent_level trans) in
        begin let str_body = ((translate_expr trans_body) body_expr) in
        ((((sprintf "begin fun %s ->\n%s\n%s") str_param) ((indent trans_body) str_body)) ((indent trans) "end"))
        end
        end
        end
      | (Expr.App (fun_expr, arg_expr)) ->
        begin let str_fun = ((translate_expr trans) fun_expr) in
        begin let str_arg = ((translate_expr trans) arg_expr) in
        (((sprintf "(%s %s)") str_fun) str_arg)
        end
        end
      | (Expr.Ctor (ctor, opt_arg_expr)) ->
        begin let str_ctor = (Names.show_ctor ctor) in
        begin match opt_arg_expr with
          | (None _) ->
            str_ctor
          | (Some (arg_expr)) ->
            begin let str_arg = ((translate_expr trans) arg_expr) in
            (((sprintf "(%s %s)") str_ctor) str_arg)
            end
        end
        end
      | (Expr.If (cond_expr, then_expr, else_expr)) ->
        begin let str_cond = ((translate_expr trans) cond_expr) in
        begin let trans_then_else = (incr_indent_level trans) in
        begin let str_then = ((translate_expr trans_then_else) then_expr) in
        begin let str_else = ((translate_expr trans_then_else) else_expr) in
        ((((((sprintf "begin if %s then\n%s\n%s\n%s\n%s") str_cond) ((indent trans_then_else) str_then)) ((indent trans) "else")) ((indent trans_then_else) str_else)) ((indent trans) "end"))
        end
        end
        end
        end
      | (Expr.Tuple ((( :: ) (x, xs)))) ->
        begin let str_x = ((translate_expr trans) x) in
        begin let str_x_xs = (((YzList.fold_left str_x) xs) begin fun acc ->
          begin fun elem ->
            (((sprintf "%s, %s") acc) ((translate_expr trans) elem))
          end
        end) in
        ((sprintf "(%s)") str_x_xs)
        end
        end
      | (Expr.Tuple (([] _))) ->
        (assert false)
      | (Expr.Record (field_defs)) ->
        begin let trans_field_def = (incr_indent_level trans) in
        begin let str_field_defs = (((YzList.fold_left "") field_defs) begin fun acc ->
          begin fun elem ->
            begin let str_field_def = ((translate_field_def trans_field_def) elem) in
            (((sprintf "%s%s;\n") acc) ((indent trans_field_def) str_field_def))
            end
          end
        end) in
        (((sprintf "{\n%s%s") str_field_defs) ((indent trans) "}"))
        end
        end
      | (Expr.Update (expr, field_defs)) ->
        begin let trans_field_def = (incr_indent_level trans) in
        begin let str_expr = ((translate_expr trans_field_def) expr) in
        begin let str_field_defs = (((YzList.fold_left "") field_defs) begin fun acc ->
          begin fun elem ->
            begin let str_field_def = ((translate_field_def trans_field_def) elem) in
            (((sprintf "%s%s;\n") acc) ((indent trans_field_def) str_field_def))
            end
          end
        end) in
        ((((sprintf "{\n%s with\n%s%s") ((indent trans_field_def) str_expr)) str_field_defs) ((indent trans) "}"))
        end
        end
        end
      | (Expr.Match (target_expr, cases)) ->
        begin let str_target = ((translate_expr trans) target_expr) in
        begin let trans_case = (incr_indent_level trans) in
        begin let str_cases = (((YzList.fold_left "") cases) begin fun acc ->
          begin fun elem ->
            (((sprintf "%s%s") acc) ((translate_case trans_case) elem))
          end
        end) in
        ((((sprintf "begin match %s with%s\n%s") str_target) str_cases) ((indent trans) "end"))
        end
        end
        end
      | (Expr.LetVal (pat, val_expr, cont_expr)) ->
        begin let str_pat = (translate_pattern pat) in
        begin let str_val = ((translate_expr trans) val_expr) in
        begin let str_cont = ((translate_expr trans) cont_expr) in
        (((((sprintf "begin let %s = %s in\n%s\n%s") str_pat) str_val) ((indent trans) str_cont)) ((indent trans) "end"))
        end
        end
        end
      | (Expr.LetFun ((( :: ) ((name, val_expr), defs)), cont_expr)) ->
        begin let str_name = (Names.show_val_name name) in
        begin let str_val = ((translate_expr trans) val_expr) in
        begin let str_let_rec = (((sprintf "let rec %s = %s") str_name) str_val) in
        begin let str_let_rec = (((YzList.fold_left str_let_rec) defs) begin fun acc ->
          begin fun (name, val_expr) ->
            begin let str_name = (Names.show_val_name name) in
            begin let str_val = ((translate_expr trans) val_expr) in
            ((((sprintf "%s\nand %s = %s") acc) str_name) str_val)
            end
            end
          end
        end) in
        begin let str_cont = ((translate_expr trans) cont_expr) in
        ((((sprintf "begin %s in\n%s\n%s") str_let_rec) ((indent trans) str_cont)) ((indent trans) "end"))
        end
        end
        end
        end
        end
      | (Expr.LetFun (([] _), cont_expr)) ->
        (assert false)
      | (Expr.Or (lhs, rhs)) ->
        begin let str_lhs = ((translate_expr trans) lhs) in
        begin let str_rhs = ((translate_expr trans) rhs) in
        (((sprintf "(%s || %s)") str_lhs) str_rhs)
        end
        end
      | (Expr.And (lhs, rhs)) ->
        begin let str_lhs = ((translate_expr trans) lhs) in
        begin let str_rhs = ((translate_expr trans) rhs) in
        (((sprintf "(%s && %s)") str_lhs) str_rhs)
        end
        end
      | (Expr.Seq (lhs, rhs)) ->
        begin let str_lhs = ((translate_expr trans) lhs) in
        begin let str_rhs = ((translate_expr trans) rhs) in
        ((((sprintf "begin\n%s;\n%s\n%s") ((indent trans) str_lhs)) ((indent trans) str_rhs)) ((indent trans) "end"))
        end
        end
      | (Expr.Field (expr, path)) ->
        begin let str_expr = ((translate_expr trans) expr) in
        begin let str_path = (Names.show_val_path path) in
        (((sprintf "%s.%s") str_expr) str_path)
        end
        end
      | (Expr.Assign (lhs, rhs)) ->
        begin let str_lhs = ((translate_expr trans) lhs) in
        begin let str_rhs = ((translate_expr trans) rhs) in
        (((sprintf "(%s <- %s)") str_lhs) str_rhs)
        end
        end
      | (Expr.Try (expr, cases)) ->
        begin let trans_expr = (incr_indent_level trans) in
        begin let str_expr = ((translate_expr trans_expr) expr) in
        begin let str_cases = (((YzList.fold_left "") cases) begin fun acc ->
          begin fun elem ->
            (((sprintf "%s%s") acc) ((translate_case trans_expr) elem))
          end
        end) in
        (((((sprintf "begin try\n%s\n%s\n%s\n%s") ((indent trans_expr) str_expr)) ((indent trans) "with")) str_cases) ((indent trans) "end"))
        end
        end
        end
    end
  end
end

and translate_field_def = begin fun trans ->
  begin fun (path, expr) ->
    begin let str_path = (Names.show_val_path path) in
    begin let str_expr = ((translate_expr trans) expr) in
    (((sprintf "%s = %s") str_path) str_expr)
    end
    end
  end
end

and translate_case = begin fun trans ->
  begin fun c ->
    begin match c with
      | (pat, (None _), body_expr) ->
        begin let str_pat = ((sprintf "| %s ->") (translate_pattern pat)) in
        begin let trans_body = (incr_indent_level trans) in
        begin let str_body = ((translate_expr trans_body) body_expr) in
        (((sprintf "\n%s\n%s") ((indent trans) str_pat)) ((indent trans_body) str_body))
        end
        end
        end
      | (pat, (Some (guard)), body_expr) ->
        begin let str_guard = ((translate_expr trans) guard) in
        begin let str_pat = (((sprintf "| %s when %s ->") (translate_pattern pat)) str_guard) in
        begin let trans_body = (incr_indent_level trans) in
        begin let str_body = ((translate_expr trans_body) body_expr) in
        (((sprintf "\n%s\n%s") ((indent trans) str_pat)) ((indent trans_body) str_body))
        end
        end
        end
        end
    end
  end
end

let rec translate_type_expr = begin fun t ->
  begin match t.TypeExpr.raw with
    | (TypeExpr.Con (typector)) ->
      (Names.show_typector typector)
    | (TypeExpr.Var (name)) ->
      ((sprintf "'%s") name)
    | (TypeExpr.App (typector, (( :: ) (t, ts)))) ->
      begin let str_typector = (Names.show_typector typector) in
      begin let str_types = (((YzList.fold_left (translate_type_expr t)) ts) begin fun acc ->
        begin fun elem ->
          (((sprintf "%s, %s") acc) (translate_type_expr elem))
        end
      end) in
      (((sprintf "(%s) %s") str_types) str_typector)
      end
      end
    | (TypeExpr.App (typector, ([] _))) ->
      (assert false)
    | (TypeExpr.Tuple ((( :: ) (t, ts)))) ->
      begin let str_types = (((YzList.fold_left (translate_type_expr t)) ts) begin fun acc ->
        begin fun elem ->
          (((sprintf "%s * %s") acc) (translate_type_expr elem))
        end
      end) in
      ((sprintf "(%s)") str_types)
      end
    | (TypeExpr.Tuple (([] _))) ->
      (assert false)
    | (TypeExpr.Fun (t1, t2)) ->
      begin let str_t1 = (translate_type_expr t1) in
      begin let str_t2 = (translate_type_expr t2) in
      (((sprintf "(%s -> %s)") str_t1) str_t2)
      end
      end
  end
end

let rec translate_ctor_decl = begin fun ctor_decl ->
  begin match ctor_decl with
    | (ctor_name, (None _)) ->
      ((sprintf "| %s\n") (Names.show_ctor_name ctor_name))
    | (ctor_name, (Some (t))) ->
      begin let str_type = (translate_type_expr t) in
      (((sprintf "| %s of %s\n") (Names.show_ctor_name ctor_name)) str_type)
      end
  end
end

let rec translate_field_decl = begin fun (is_mutable, field_name, t) ->
  begin if is_mutable then
    (((sprintf "mutable %s : %s;\n") (Names.show_val_name field_name)) (translate_type_expr t))
  else
    (((sprintf "%s : %s;\n") (Names.show_val_name field_name)) (translate_type_expr t))
  end
end

let rec translate_exn_decl = begin fun exn_decl ->
  begin match exn_decl with
    | (ctor_name, (None _)) ->
      ((sprintf "%s") (Names.show_ctor_name ctor_name))
    | (ctor_name, (Some (t))) ->
      begin let str_type = (translate_type_expr t) in
      (((sprintf "%s of %s") (Names.show_ctor_name ctor_name)) str_type)
      end
  end
end

let rec translate_type_info = begin fun trans ->
  begin fun type_info ->
    begin match type_info with
      | (TypeInfo.Abbrev (t)) ->
        (translate_type_expr t)
      | (TypeInfo.Variant (ctor_decls)) ->
        begin let trans_ctor_decl = (incr_indent_level trans) in
        begin let str_ctor_decls = (((YzList.fold_left "") ctor_decls) begin fun acc ->
          begin fun elem ->
            (((sprintf "%s%s") acc) ((indent trans_ctor_decl) (translate_ctor_decl elem)))
          end
        end) in
        ((sprintf "\n%s") str_ctor_decls)
        end
        end
      | (TypeInfo.Record (field_decls)) ->
        begin let trans_field_decl = (incr_indent_level trans) in
        begin let str_field_decls = (((YzList.fold_left "") field_decls) begin fun acc ->
          begin fun elem ->
            (((sprintf "%s%s") acc) ((indent trans_field_decl) (translate_field_decl elem)))
          end
        end) in
        (((sprintf "{\n%s%s") str_field_decls) ((indent trans) "}"))
        end
        end
    end
  end
end

let rec translate_top = begin fun trans ->
  begin fun top ->
    begin match top.Top.raw with
      | (Top.Expr (expr)) ->
        begin let str_expr = ((translate_expr trans) expr) in
        ((sprintf "let () = %s\n") str_expr)
        end
      | (Top.LetVal (pat, expr)) ->
        begin let str_pat = (translate_pattern pat) in
        begin let str_expr = ((translate_expr trans) expr) in
        (((sprintf "let %s = %s\n") str_pat) str_expr)
        end
        end
      | (Top.LetFun ((( :: ) ((name, expr), defs)))) ->
        begin let str_name = (Names.show_val_name name) in
        begin let str_expr = ((translate_expr trans) expr) in
        begin let str_let_rec = (((sprintf "let rec %s = %s\n") str_name) str_expr) in
        (((YzList.fold_left str_let_rec) defs) begin fun acc ->
          begin fun (name, expr) ->
            begin let str_name = (Names.show_val_name name) in
            begin let str_expr = ((translate_expr trans) expr) in
            ((((sprintf "%s\nand %s = %s\n") acc) str_name) str_expr)
            end
            end
          end
        end)
        end
        end
        end
      | (Top.LetFun (([] _))) ->
        (assert false)
      | (Top.Open (path)) ->
        begin let str_path = (Names.show_mod_path path) in
        ((sprintf "open %s\n") str_path)
        end
      | (Top.Type ((( :: ) ((name, info), defs)))) ->
        begin let str_info = ((translate_type_info trans) info) in
        begin let str_type = (((sprintf "type %s = %s\n") name) str_info) in
        (((YzList.fold_left str_type) defs) begin fun acc ->
          begin fun (name, info) ->
            begin let str_info = ((translate_type_info trans) info) in
            ((((sprintf "%s\nand %s = %s\n") acc) name) str_info)
            end
          end
        end)
        end
        end
      | (Top.Type (([] _))) ->
        (assert false)
      | (Top.Module (name, ftor, arg)) ->
        begin let str_name = name in
        begin let str_ftor = (Names.show_mod_path ftor) in
        begin let str_arg = (Names.show_mod_path arg) in
        ((((sprintf "module %s = %s(%s)\n") str_name) str_ftor) str_arg)
        end
        end
        end
      | (Top.Exception (exn_decl)) ->
        begin let str_exn_decl = (translate_exn_decl exn_decl) in
        ((sprintf "exception %s\n") str_exn_decl)
        end
    end
  end
end

