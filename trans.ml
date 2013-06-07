open Printf

type t = {
  basic_offset : int;
  indent_level : int;
}

let rec incr_indent_level = begin fun {basic_offset;indent_level;} ->
  {
    basic_offset = basic_offset;
    indent_level = ((( + ) indent_level) 1);
  }
end

let rec create = begin fun basic_offset ->
  {
    basic_offset = basic_offset;
    indent_level = 0;
  }
end

let ocaml_basic_offset = 2

let rec indent = begin fun {basic_offset;indent_level;} ->
  begin fun str ->
    begin let offset = ((( * ) basic_offset) indent_level) in
    (((sprintf "%s%s") ((String.make offset) ' ')) str)
    end
  end
end

let rec translate_val_name = begin fun name ->
  begin match name with
    | (Names.Id(str)) ->
      str
    | (Names.Op(str)) ->
      ((sprintf "( %s )") str)
  end
end

let translate_ctor_name = translate_val_name

let rec translate_mod_path = begin fun path ->
  begin match path with
    | ([](_)) ->
      ""
    | (( :: )(name, names)) ->
      (((List.fold_left begin fun acc ->
        begin fun elem ->
          (((sprintf "%s.%s") acc) elem)
        end
      end) name) names)
  end
end

let rec translate_val_path = begin fun path ->
  begin match path with
    | (([](_)), val_name) ->
      (translate_val_name val_name)
    | (mod_path, val_name) ->
      (((sprintf "%s.%s") (translate_mod_path mod_path)) (translate_val_name val_name))
  end
end

let translate_ctor = translate_val_path

let rec translate_literal = begin fun lit ->
  begin match lit with
    | (Literal.Int(n)) ->
      ((sprintf "%d") n)
    | (Literal.String(str)) ->
      ((sprintf "\"%s\"") str)
    | (Literal.Char(str)) ->
      ((sprintf "'%s'") str)
  end
end

let rec translate_pattern = begin fun pat ->
  begin match pat with
    | (Pattern.Con(lit)) ->
      (translate_literal lit)
    | (Pattern.Var(name)) ->
      (translate_val_name name)
    | (Pattern.Variant(ctor, (( :: )(pat, pats)))) ->
      begin let str_ctor = (translate_ctor ctor) in
      begin let str_pat_list = (((List.fold_left begin fun acc ->
        begin fun elem ->
          (((sprintf "%s, %s") acc) (translate_pattern elem))
        end
      end) (translate_pattern pat)) pats) in
      (((sprintf "(%s(%s))") str_ctor) str_pat_list)
      end
      end
    | (Pattern.Variant(ctor, ([](_)))) ->
      (assert false)
    | (Pattern.Tuple((( :: )(pat, pats)))) ->
      begin let str_pat_list = (((List.fold_left begin fun acc ->
        begin fun elem ->
          (((sprintf "%s, %s") acc) (translate_pattern elem))
        end
      end) (translate_pattern pat)) pats) in
      ((sprintf "(%s)") str_pat_list)
      end
    | (Pattern.Tuple(([](_)))) ->
      (assert false)
    | (Pattern.Record(fields)) ->
      begin let str_fields = (((List.fold_left begin fun acc ->
        begin fun elem ->
          (((sprintf "%s%s;") acc) (translate_field_def elem))
        end
      end) "") fields) in
      ((sprintf "{%s}") str_fields)
      end
    | (Pattern.Or(lhs, rhs)) ->
      begin let str_lhs = (translate_pattern lhs) in
      begin let str_rhs = (translate_pattern rhs) in
      (((sprintf "(%s | %s)") str_lhs) str_rhs)
      end
      end
    | (Pattern.As(pat, name)) ->
      begin let str_pat = (translate_pattern pat) in
      begin let str_name = (translate_val_name name) in
      (((sprintf "(%s as %s)") str_pat) str_name)
      end
      end
  end
end

and translate_field_def = begin fun field_def ->
  begin match field_def with
    | (path, (None(_))) ->
      ((sprintf "%s") (translate_val_path path))
    | (path, (Some(pat))) ->
      (((sprintf "%s=%s") (translate_val_path path)) (translate_pattern pat))
  end
end

let rec translate_expr = begin fun trans ->
  begin fun expr ->
    begin match expr with
      | (Expr.Con(lit)) ->
        (translate_literal lit)
      | (Expr.Var(path)) ->
        (translate_val_path path)
      | (Expr.Ctor(ctor)) ->
        (translate_ctor ctor)
      | (Expr.Abs(param_pat, body_expr)) ->
        begin let str_param = (translate_pattern param_pat) in
        begin let trans_body = (incr_indent_level trans) in
        begin let str_body = ((translate_expr trans_body) body_expr) in
        ((((sprintf "begin fun %s ->\n%s\n%s") str_param) ((indent trans_body) str_body)) ((indent trans) "end"))
        end
        end
        end
      | (Expr.App(fun_expr, arg_expr)) ->
        begin let str_fun = ((translate_expr trans) fun_expr) in
        begin let str_arg = ((translate_expr trans) arg_expr) in
        (((sprintf "(%s %s)") str_fun) str_arg)
        end
        end
      | (Expr.If(cond_expr, then_expr, else_expr)) ->
        begin let str_cond = ((translate_expr trans) cond_expr) in
        begin let trans_then_else = (incr_indent_level trans) in
        begin let str_then = ((translate_expr trans_then_else) then_expr) in
        begin let str_else = ((translate_expr trans_then_else) else_expr) in
        ((((((sprintf "begin if %s then\n%s\n%s\n%s\n%s") str_cond) ((indent trans_then_else) str_then)) ((indent trans) "else")) ((indent trans_then_else) str_else)) ((indent trans) "end"))
        end
        end
        end
        end
      | (Expr.Tuple((( :: )(x, xs)))) ->
        begin let str_x = ((translate_expr trans) x) in
        begin let str_x_xs = (((List.fold_left begin fun acc ->
          begin fun elem ->
            (((sprintf "%s, %s") acc) ((translate_expr trans) elem))
          end
        end) str_x) xs) in
        ((sprintf "(%s)") str_x_xs)
        end
        end
      | (Expr.Tuple(([](_)))) ->
        (assert false)
      | (Expr.Record(field_defs)) ->
        begin let trans_field_def = (incr_indent_level trans) in
        begin let str_field_defs = (((List.fold_left begin fun acc ->
          begin fun elem ->
            begin let str_field_def = ((translate_field_def trans_field_def) elem) in
            (((sprintf "%s%s;\n") acc) ((indent trans_field_def) str_field_def))
            end
          end
        end) "") field_defs) in
        (((sprintf "{\n%s%s") str_field_defs) ((indent trans) "}"))
        end
        end
      | (Expr.Match(target_expr, cases)) ->
        begin let str_target = ((translate_expr trans) target_expr) in
        begin let trans_case = (incr_indent_level trans) in
        begin let str_cases = (((List.fold_left begin fun acc ->
          begin fun elem ->
            (((sprintf "%s%s") acc) ((translate_case trans_case) elem))
          end
        end) "") cases) in
        ((((sprintf "begin match %s with%s\n%s") str_target) str_cases) ((indent trans) "end"))
        end
        end
        end
      | (Expr.LetVal(pat, val_expr, cont_expr)) ->
        begin let str_pat = (translate_pattern pat) in
        begin let str_val = ((translate_expr trans) val_expr) in
        begin let str_cont = ((translate_expr trans) cont_expr) in
        (((((sprintf "begin let %s = %s in\n%s\n%s") str_pat) str_val) ((indent trans) str_cont)) ((indent trans) "end"))
        end
        end
        end
      | (Expr.LetFun((( :: )((name, val_expr), defs)), cont_expr)) ->
        begin let str_name = (Names.show_val_name name) in
        begin let str_val = ((translate_expr trans) val_expr) in
        begin let str_let_rec = (((sprintf "let rec %s = %s") str_name) str_val) in
        begin let str_let_rec = (((List.fold_left begin fun acc ->
          begin fun (name, val_expr) ->
            begin let str_name = (translate_val_name name) in
            begin let str_val = ((translate_expr trans) val_expr) in
            ((((sprintf "%s\nand %s = %s") acc) str_name) str_val)
            end
            end
          end
        end) str_let_rec) defs) in
        begin let str_cont = ((translate_expr trans) cont_expr) in
        ((((sprintf "begin %s in\n%s\n%s") str_let_rec) ((indent trans) str_cont)) ((indent trans) "end"))
        end
        end
        end
        end
        end
      | (Expr.LetFun(([](_)), cont_expr)) ->
        (assert false)
      | (Expr.Seq(lhs, rhs)) ->
        begin let str_lhs = ((translate_expr trans) lhs) in
        begin let str_rhs = ((translate_expr trans) rhs) in
        ((((sprintf "begin\n%s;\n%s\n%s") ((indent trans) str_lhs)) ((indent trans) str_rhs)) ((indent trans) "end"))
        end
        end
      | (Expr.Field(expr, path)) ->
        begin let str_expr = ((translate_expr trans) expr) in
        begin let str_path = (translate_val_path path) in
        (((sprintf "%s.%s") str_expr) str_path)
        end
        end
      | (Expr.Assign(lhs, rhs)) ->
        begin let str_lhs = ((translate_expr trans) lhs) in
        begin let str_rhs = ((translate_expr trans) rhs) in
        (((sprintf "(%s <- %s)") str_lhs) str_rhs)
        end
        end
      | (Expr.Try(expr, cases)) ->
        begin let trans_expr = (incr_indent_level trans) in
        begin let str_expr = ((translate_expr trans_expr) expr) in
        begin let str_cases = (((List.fold_left begin fun acc ->
          begin fun elem ->
            (((sprintf "%s%s") acc) ((translate_case trans_expr) elem))
          end
        end) "") cases) in
        (((((sprintf "begin try\n%s\n%s\n%s\n%s") ((indent trans_expr) str_expr)) ((indent trans) "with")) str_cases) ((indent trans) "end"))
        end
        end
        end
    end
  end
end

and translate_field_def = begin fun trans ->
  begin fun (path, expr) ->
    begin let str_path = (translate_val_path path) in
    begin let str_expr = ((translate_expr trans) expr) in
    (((sprintf "%s = %s") str_path) str_expr)
    end
    end
  end
end

and translate_case = begin fun trans ->
  begin fun c ->
    begin match c with
      | (pat, (None(_)), body_expr) ->
        begin let str_pat = ((sprintf "| %s ->") (translate_pattern pat)) in
        begin let trans_body = (incr_indent_level trans) in
        begin let str_body = ((translate_expr trans_body) body_expr) in
        (((sprintf "\n%s\n%s") ((indent trans) str_pat)) ((indent trans_body) str_body))
        end
        end
        end
      | (pat, (Some(guard)), body_expr) ->
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

let rec translate_typector = begin fun typector ->
  begin match typector with
    | (([](_)), typector_name) ->
      typector_name
    | (mod_path, typector_name) ->
      (((sprintf "%s.%s") (translate_mod_path mod_path)) typector_name)
  end
end

let rec translate_type = begin fun t ->
  begin match t with
    | (Type.Con(typector)) ->
      (translate_typector typector)
    | (Type.App(typector, (( :: )(t, ts)))) ->
      begin let str_typector = (translate_typector typector) in
      begin let str_types = (((List.fold_left begin fun acc ->
        begin fun elem ->
          (((sprintf "%s, %s") acc) (translate_type elem))
        end
      end) (translate_type t)) ts) in
      (((sprintf "(%s) %s") str_types) str_typector)
      end
      end
    | (Type.App(typector, ([](_)))) ->
      (assert false)
    | (Type.Tuple((( :: )(t, ts)))) ->
      begin let str_types = (((List.fold_left begin fun acc ->
        begin fun elem ->
          (((sprintf "%s * %s") acc) (translate_type elem))
        end
      end) (translate_type t)) ts) in
      ((sprintf "(%s)") str_types)
      end
    | (Type.Tuple(([](_)))) ->
      (assert false)
  end
end

let rec translate_ctor_decl = begin fun ctor_decl ->
  begin match ctor_decl with
    | (ctor_name, (None(_))) ->
      ((sprintf "| %s\n") (translate_ctor_name ctor_name))
    | (ctor_name, (Some(t))) ->
      begin let str_type = (translate_type t) in
      (((sprintf "| %s of %s\n") (translate_ctor_name ctor_name)) str_type)
      end
  end
end

let rec translate_field_decl = begin fun (is_mutable, field_name, t) ->
  begin if is_mutable then
    (((sprintf "mutable %s : %s;\n") (translate_val_name field_name)) (translate_type t))
  else
    (((sprintf "%s : %s;\n") (translate_val_name field_name)) (translate_type t))
  end
end

let rec translate_exn_decl = begin fun exn_decl ->
  begin match exn_decl with
    | (ctor_name, (None(_))) ->
      ((sprintf "%s") (translate_ctor_name ctor_name))
    | (ctor_name, (Some(t))) ->
      begin let str_type = (translate_type t) in
      (((sprintf "%s of %s") (translate_ctor_name ctor_name)) str_type)
      end
  end
end

let rec translate_top = begin fun trans ->
  begin fun top ->
    begin match top with
      | (Top.Expr(expr)) ->
        begin let str_expr = ((translate_expr trans) expr) in
        ((sprintf "let () = %s\n") str_expr)
        end
      | (Top.LetVal(pat, expr)) ->
        begin let str_pat = (translate_pattern pat) in
        begin let str_expr = ((translate_expr trans) expr) in
        (((sprintf "let %s = %s\n") str_pat) str_expr)
        end
        end
      | (Top.LetFun((( :: )((name, expr), defs)))) ->
        begin let str_name = (translate_val_name name) in
        begin let str_expr = ((translate_expr trans) expr) in
        begin let str_let_rec = (((sprintf "let rec %s = %s\n") str_name) str_expr) in
        (((List.fold_left begin fun acc ->
          begin fun (name, expr) ->
            begin let str_name = (translate_val_name name) in
            begin let str_expr = ((translate_expr trans) expr) in
            ((((sprintf "%s\nand %s = %s\n") acc) str_name) str_expr)
            end
            end
          end
        end) str_let_rec) defs)
        end
        end
        end
      | (Top.LetFun(([](_)))) ->
        (assert false)
      | (Top.Open(path)) ->
        begin let str_path = (translate_mod_path path) in
        ((sprintf "open %s\n") str_path)
        end
      | (Top.Abbrev(name, t)) ->
        begin let str_type = (translate_type t) in
        (((sprintf "type %s = %s\n") name) str_type)
        end
      | (Top.Variant(name, ctor_decls)) ->
        begin let trans_ctor_decl = (incr_indent_level trans) in
        begin let str_ctor_decls = (((List.fold_left begin fun acc ->
          begin fun elem ->
            (((sprintf "%s%s") acc) ((indent trans_ctor_decl) (translate_ctor_decl elem)))
          end
        end) "") ctor_decls) in
        (((sprintf "type %s =\n%s") name) str_ctor_decls)
        end
        end
      | (Top.Record(name, field_decls)) ->
        begin let trans_field_decl = (incr_indent_level trans) in
        begin let str_field_decls = (((List.fold_left begin fun acc ->
          begin fun elem ->
            (((sprintf "%s%s") acc) ((indent trans_field_decl) (translate_field_decl elem)))
          end
        end) "") field_decls) in
        ((((sprintf "type %s = {\n%s%s\n") name) str_field_decls) ((indent trans) "}"))
        end
        end
      | (Top.Exception(exn_decl)) ->
        begin let str_exn_decl = (translate_exn_decl exn_decl) in
        ((sprintf "exception %s\n") str_exn_decl)
        end
    end
  end
end

exception Break

let rec translate_file = begin fun fname_in ->
  begin fun fname_out ->
    begin let chan_in = (open_in fname_in) in
    begin try
      begin let chan_out = (open_out fname_out) in
      begin try
        begin let src = ((Source.create fname_in) chan_in) in
        begin let lexer = (Lexer.create src) in
        begin let parser = (Parser.create lexer) in
        begin let trans = (create ocaml_basic_offset) in
        begin let rec loop = begin fun (()(_)) ->
          begin match (Parser.parse parser) with
            | (None(_)) ->
              (raise Break)
            | (Some(top)) ->
              begin let result = ((translate_top trans) top) in
              begin
              (((fprintf chan_out) "%s\n") result);
              (loop ())
              end
              end
          end
        end in
        (loop ())
        end
        end
        end
        end
        end
      with

        | (Break(_)) ->
          begin
          (close_out chan_out);
          (raise Break)
          end
        | (Failure(message)) ->
          begin
          (close_out chan_out);
          begin
          ((eprintf "%s") message);
          begin
          (flush stderr);
          (failwith message)
          end
          end
          end
        | exn ->
          begin
          (close_out_noerr chan_out);
          (raise exn)
          end
      end
      end
    with

      | (Break(_)) ->
        begin
        (close_in chan_in);
        true
        end
      | (Failure(_)) ->
        begin
        (close_in chan_in);
        false
        end
      | exn ->
        begin
        (close_in_noerr chan_in);
        (raise exn)
        end
    end
    end
  end
end

let rec test = begin fun (()(_)) ->
  ((translate_file "test.yz") "test.out")
end

