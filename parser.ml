open Printf

type t = {
  lexer : Lexer.t;
  mutable token : Token.t;
  mutable pos : Pos.t;
}

type sep_or_term =
  | Sep
  | Term
  | Neither

let rec create = begin fun lexer ->
  {
    lexer = lexer;
    token = Token.EOF;
    pos = Pos.dummy;
  }
end

let rec expected = begin fun parser ->
  begin fun str_token ->
    ((((sprintf "%s: error: unexpected %s, expected %s\n") (Pos.show parser.pos)) (Token.show parser.token)) str_token)
  end
end

let rec lookahead = begin fun parser ->
  begin match (Lexer.next parser.lexer) with
    | ((None(_)), pos) ->
      begin
      (parser.token <- Token.EOF);
      (parser.pos <- pos)
      end
    | ((Some(token)), pos) ->
      begin
      (parser.token <- token);
      (parser.pos <- pos)
      end
  end
end

let rec skip = begin fun parser ->
  begin fun token ->
    begin if ((( = ) parser.token) token) then
      (lookahead parser)
    else
      ()
    end
  end
end

let rec make_abs = begin fun params ->
  begin fun body_expr ->
    begin let rec mk_abs = begin fun param ->
      begin fun expr ->
        (Expr.Abs (param, expr))
      end
    end in
    (((List.fold_right mk_abs) params) body_expr)
    end
  end
end

let rec make_app = begin fun fun_expr ->
  begin fun arg_exprs ->
    begin let rec mk_app = begin fun e1 ->
      begin fun e2 ->
        (Expr.App (e1, e2))
      end
    end in
    (((List.fold_left mk_app) fun_expr) arg_exprs)
    end
  end
end

let rec parse_non_assoc = begin fun parser ->
  begin fun get_op ->
    begin fun parse_lower ->
      begin let lhs = (parse_lower parser) in
      begin match (get_op parser.token) with
        | (None(_)) ->
          lhs
        | (Some(str)) ->
          begin
          (lookahead parser);
          begin let op = (Expr.Var ([], (Names.Op (str)))) in
          begin let rhs = (parse_lower parser) in
          (Expr.App ((Expr.App (op, lhs)), rhs))
          end
          end
          end
      end
      end
    end
  end
end

let rec parse_right_assoc = begin fun parser ->
  begin fun get_op ->
    begin fun parse_lower ->
      begin let lhs = (parse_lower parser) in
      begin match (get_op parser.token) with
        | (None(_)) ->
          lhs
        | (Some(str)) ->
          begin
          (lookahead parser);
          begin let op = (Expr.Var ([], ((Names.Op (str))))) in
          begin let rhs = (((parse_right_assoc parser) get_op) parse_lower) in
          (Expr.App ((Expr.App (op, lhs)), rhs))
          end
          end
          end
      end
      end
    end
  end
end

let rec parse_left_assoc = begin fun parser ->
  begin fun get_op ->
    begin fun parse_lower ->
      begin let lhs = (parse_lower parser) in
      begin let rec loop = begin fun lhs ->
        begin match (get_op parser.token) with
          | (None(_)) ->
            lhs
          | (Some(str)) ->
            begin
            (lookahead parser);
            begin let op = (Expr.Var ([], ((Names.Op (str))))) in
            begin let rhs = (parse_lower parser) in
            (loop (Expr.App ((Expr.App (op, lhs)), rhs)))
            end
            end
            end
        end
      end in
      (loop lhs)
      end
      end
    end
  end
end

let rec parse_elems = begin fun parser ->
  begin fun sep_or_term ->
    begin fun parse_elem ->
      begin let rec loop = begin fun elems ->
        begin match (sep_or_term parser.token) with
          | (Term(_)) ->
            begin
            (lookahead parser);
            (List.rev elems)
            end
          | _ ->
            begin let elem = (parse_elem parser) in
            begin match (sep_or_term parser.token) with
              | (Term(_)) ->
                begin
                (lookahead parser);
                (List.rev (( :: ) (elem, elems)))
                end
              | (Sep(_)) ->
                begin
                (lookahead parser);
                (loop (( :: ) (elem, elems)))
                end
              | (Neither(_)) ->
                (failwith ((expected parser) "separator or terminator"))
            end
            end
        end
      end in
      (loop [])
      end
    end
  end
end

let rec parse_indented_elems = begin fun parser ->
  begin fun parse_elem ->
    begin let rec sep_or_term = begin fun token ->
      begin match token with
        | (Token.Reserved(";")) ->
          Sep
        | (Token.Newline(_)) ->
          Sep
        | (Token.Undent(_)) ->
          Term
        | _ ->
          Neither
      end
    end in
    (((parse_elems parser) sep_or_term) parse_elem)
    end
  end
end

let rec parse_braced_elems = begin fun parser ->
  begin fun parse_elem ->
    begin let rec sep_or_term = begin fun token ->
      begin match token with
        | (Token.Reserved(";")) ->
          Sep
        | (Token.Reserved("}")) ->
          Term
        | _ ->
          Neither
      end
    end in
    (((parse_elems parser) sep_or_term) parse_elem)
    end
  end
end

let rec parse_block_like_elems = begin fun parser ->
  begin fun parse_elem ->
    begin match parser.token with
      | (Token.Reserved(":")) ->
        begin
        (Lexer.indent parser.lexer);
        begin
        (lookahead parser);
        ((parse_indented_elems parser) parse_elem)
        end
        end
      | (Token.Reserved("{")) ->
        begin
        (lookahead parser);
        ((parse_braced_elems parser) parse_elem)
        end
      | _ ->
        (failwith ((expected parser) "':' or '{'"))
    end
  end
end

let rec parse_top = begin fun parser ->
  begin match parser.token with
    | (Token.Reserved("def")) ->
      (Top.LetFun ((( :: ) ((parse_top_let_fun parser), []))))
    | (Token.Reserved("var")) ->
      (parse_top_let_val parser)
    | (Token.Reserved("rec")) ->
      begin
      (lookahead parser);
      (Top.LetFun (((parse_block_like_elems parser) parse_top_let_fun)))
      end
    | (Token.Reserved("open")) ->
      (parse_top_open parser)
    | (Token.Reserved("type")) ->
      (parse_top_typedef parser)
    | _ ->
      (Top.Expr ((parse_expr parser)))
  end
end
and parse_top_let_fun = begin fun parser ->
  begin
  (lookahead parser);
  begin let fun_name = (parse_val_name parser) in
  begin let params = (parse_params parser) in
  begin let body_expr = (parse_block parser) in
  (fun_name, ((make_abs params) body_expr))
  end
  end
  end
  end
end
and parse_top_let_val = begin fun parser ->
  begin
  (lookahead parser);
  begin let val_pat = (parse_pattern parser) in
  begin if ((( <> ) parser.token) (Token.CmpOp ("="))) then
    (failwith ((expected parser) "'='"))
  else
    begin
    (lookahead parser);
    (Top.LetVal (val_pat, (parse_expr parser)))
    end
  end
  end
  end
end
and parse_top_open = begin fun parser ->
  begin
  (lookahead parser);
  begin let mod_path = ((parse_mod_path parser) []) in
  (Top.Open (mod_path))
  end
  end
end
and parse_top_typedef = begin fun parser ->
  begin
  (lookahead parser);
  begin let typector_name = (parse_lowid parser) in
  begin match parser.token with
    | (Token.CmpOp("=")) ->
      begin
      (lookahead parser);
      begin let t = (parse_type parser) in
      (Top.Abbrev (typector_name, t))
      end
      end
    | ((Token.Reserved(":")) | (Token.Reserved("{"))) ->
      ((parse_type_repr parser) typector_name)
    | _ ->
      (failwith ((expected parser) "'=' or ':' or '{'"))
  end
  end
  end
end
and parse_type_repr = begin fun parser ->
  begin fun typector_name ->
    begin match parser.token with
      | (Token.Reserved(":")) ->
        begin
        (Lexer.indent parser.lexer);
        begin
        (lookahead parser);
        begin match parser.token with
          | (Token.Reserved("def")) ->
            (Top.Variant (typector_name, ((parse_indented_elems parser) parse_ctor_decl)))
          | _ ->
            (Top.Record (typector_name, ((parse_indented_elems parser) parse_field_decl)))
        end
        end
        end
      | (Token.Reserved("{")) ->
        begin
        (lookahead parser);
        begin match parser.token with
          | (Token.Reserved("def")) ->
            (Top.Variant (typector_name, ((parse_braced_elems parser) parse_ctor_decl)))
          | _ ->
            (Top.Record (typector_name, ((parse_braced_elems parser) parse_field_decl)))
        end
        end
      | _ ->
        (failwith ((expected parser) "':' or '{'"))
    end
  end
end
and parse_ctor_decl = begin fun parser ->
  begin
  (lookahead parser);
  begin let ctor_name = (Names.Id ((parse_capid parser))) in
  begin if ((( <> ) parser.token) (Token.Reserved ("("))) then
    (ctor_name, None)
  else
    begin
    (lookahead parser);
    begin let t = (parse_type parser) in
    begin if ((( <> ) parser.token) (Token.Reserved (")"))) then
      (failwith ((expected parser) "')'"))
    else
      begin
      (lookahead parser);
      (ctor_name, (Some (t)))
      end
    end
    end
    end
  end
  end
  end
end
and parse_field_decl = begin fun parser ->
  begin let is_mutable = (((( = ) parser.token) (Token.Reserved ("mutable")))) in
  begin
  begin if is_mutable then
    (lookahead parser)
  else
    ()
  end;
  begin let field_name = (parse_val_name parser) in
  begin if ((( <> ) parser.token) (Token.Reserved (":"))) then
    (failwith ((expected parser) "':'"))
  else
    begin
    (lookahead parser);
    begin let t = (parse_type parser) in
    (is_mutable, field_name, t)
    end
    end
  end
  end
  end
  end
end
and parse_type = begin fun parser ->
  (parse_tuple_type parser)
end
and parse_tuple_type = begin fun parser ->
  begin let t = (parse_atomic_type parser) in
  begin let rec loop = begin fun ts ->
    begin match parser.token with
      | (Token.MulOp("*")) ->
        begin
        (lookahead parser);
        begin let t = (parse_atomic_type parser) in
        (loop (( :: ) (t, ts)))
        end
        end
      | _ ->
        (List.rev ts)
    end
  end in
  begin match parser.token with
    | (Token.MulOp("*")) ->
      (Type.Tuple ((loop (( :: ) (t, [])))))
    | _ ->
      t
  end
  end
  end
end
and parse_atomic_type = begin fun parser ->
  begin match parser.token with
    | ((Token.LowId(_)) | (Token.CapId(_))) ->
      begin let typector = ((parse_typector parser) []) in
      begin match parser.token with
        | (Token.Reserved("(")) ->
          begin
          (lookahead parser);
          begin let args = (parse_type_args parser) in
          (Type.App (typector, args))
          end
          end
        | _ ->
          (Type.Con (typector))
      end
      end
    | (Token.Reserved("(")) ->
      begin
      (lookahead parser);
      begin let t = (parse_type parser) in
      begin if ((( <> ) parser.token) (Token.Reserved (")"))) then
        (failwith ((expected parser) "')'"))
      else
        begin
        (lookahead parser);
        t
        end
      end
      end
      end
    | _ ->
      (failwith ((expected parser) "type"))
  end
end
and parse_typector = begin fun parser ->
  begin fun mod_names ->
    begin match parser.token with
      | (Token.LowId(_)) ->
        begin let typector_name = (parse_lowid parser) in
        ((List.rev mod_names), typector_name)
        end
      | (Token.CapId(_)) ->
        begin let capid = (parse_capid parser) in
        begin if ((( <> ) parser.token) (Token.Reserved ("."))) then
          (failwith ((expected parser) "'.'"))
        else
          begin
          (lookahead parser);
          ((parse_typector parser) (( :: ) (capid, mod_names)))
          end
        end
        end
      | _ ->
        (failwith ((expected parser) "identifier"))
    end
  end
end
and parse_type_args = begin fun parser ->
  begin let rec sep_or_term = begin fun token ->
    begin match token with
      | (Token.Reserved(",")) ->
        Sep
      | (Token.Reserved(")")) ->
        Term
      | _ ->
        Neither
    end
  end in
  (((parse_elems parser) sep_or_term) parse_type)
  end
end
and parse_expr = begin fun parser ->
  (parse_assign_expr parser)
end
and parse_assign_expr = begin fun parser ->
  begin let lhs = (parse_or_expr parser) in
  begin match parser.token with
    | (Token.AssignOp("<-")) ->
      begin
      (lookahead parser);
      begin let rhs = (parse_assign_expr parser) in
      (Expr.Assign (lhs, rhs))
      end
      end
    | _ ->
      lhs
  end
  end
end
and parse_or_expr = begin fun parser ->
  begin let rec get_op = begin fun token ->
    begin match token with
      | (Token.OrOp(str)) ->
        (Some (str))
      | _ ->
        None
    end
  end in
  (((parse_right_assoc parser) get_op) parse_and_expr)
  end
end
and parse_and_expr = begin fun parser ->
  begin let rec get_op = begin fun token ->
    begin match token with
      | (Token.AndOp(str)) ->
        (Some (str))
      | _ ->
        None
    end
  end in
  (((parse_right_assoc parser) get_op) parse_cmp_expr)
  end
end
and parse_cmp_expr = begin fun parser ->
  begin let rec get_op = begin fun token ->
    begin match token with
      | (Token.CmpOp(str)) ->
        (Some (str))
      | _ ->
        None
    end
  end in
  (((parse_non_assoc parser) get_op) parse_cons_expr)
  end
end
and parse_cons_expr = begin fun parser ->
  begin let lhs = (parse_add_expr parser) in
  begin match parser.token with
    | (Token.ConsOp(str)) ->
      begin
      (lookahead parser);
      begin let op = (Expr.Var ([], ((Names.Op (str))))) in
      begin let rhs = (parse_cons_expr parser) in
      (Expr.App (op, (Expr.Tuple ((( :: ) (lhs, (( :: ) (rhs, []))))))))
      end
      end
      end
    | _ ->
      lhs
  end
  end
end
and parse_add_expr = begin fun parser ->
  begin let rec get_op = begin fun token ->
    begin match token with
      | (Token.AddOp(str)) ->
        (Some (str))
      | _ ->
        None
    end
  end in
  (((parse_left_assoc parser) get_op) parse_mul_expr)
  end
end
and parse_mul_expr = begin fun parser ->
  begin let rec get_op = begin fun token ->
    begin match token with
      | (Token.MulOp(str)) ->
        (Some (str))
      | _ ->
        None
    end
  end in
  (((parse_left_assoc parser) get_op) parse_pow_expr)
  end
end
and parse_pow_expr = begin fun parser ->
  begin let rec get_op = begin fun token ->
    begin match token with
      | (Token.PowOp(str)) ->
        (Some (str))
      | _ ->
        None
    end
  end in
  (((parse_right_assoc parser) get_op) parse_unary_expr)
  end
end
and parse_unary_expr = begin fun parser ->
  begin match parser.token with
    | (Token.AddOp("-")) ->
      begin
      (lookahead parser);
      begin let expr = (parse_unary_expr parser) in
      (Expr.App ((Expr.Var ([], ((Names.Op ("~-"))))), expr))
      end
      end
    | (Token.AddOp("+")) ->
      begin
      (lookahead parser);
      begin let expr = (parse_unary_expr parser) in
      (Expr.App ((Expr.Var ([], ((Names.Op ("~+"))))), expr))
      end
      end
    | _ ->
      (parse_prim_expr parser)
  end
end
and parse_prim_expr = begin fun parser ->
  begin let fun_expr = (parse_dot_expr parser) in
  begin let rec loop = begin fun fun_expr ->
    begin match parser.token with
      | (Token.Reserved("(")) ->
        begin
        (lookahead parser);
        begin let arg_exprs = (parse_args parser) in
        (loop ((make_app fun_expr) arg_exprs))
        end
        end
      | (Token.Reserved("^")) ->
        begin let arg_expr = (parse_abs parser) in
        (loop (Expr.App (fun_expr, arg_expr)))
        end
      | _ ->
        fun_expr
    end
  end in
  (loop fun_expr)
  end
  end
end
and parse_dot_expr = begin fun parser ->
  begin let expr = (parse_atomic_expr parser) in
  begin match parser.token with
    | (Token.Reserved(".")) ->
      begin
      (lookahead parser);
      begin let path = ((parse_val_path parser) []) in
      (Expr.Field (expr, path))
      end
      end
    | _ ->
      expr
  end
  end
end
and parse_atomic_expr = begin fun parser ->
  begin match parser.token with
    | (((Token.Int(_)) | (Token.String(_))) | (Token.Char(_))) ->
      begin let lit = (parse_literal parser) in
      (Expr.Con (lit))
      end
    | (((Token.LowId(_)) | (Token.CapId(_))) | (Token.Reserved("$"))) ->
      ((parse_var_or_ctor_app parser) [])
    | (Token.Reserved("^")) ->
      (parse_abs parser)
    | (Token.Reserved("if")) ->
      (parse_if_expr parser)
    | (Token.Reserved("[")) ->
      (parse_list parser)
    | (Token.Reserved("{")) ->
      (parse_record parser)
    | (Token.Reserved("(")) ->
      (parse_parens parser)
    | (Token.Reserved("match")) ->
      (parse_match_expr parser)
    | _ ->
      (failwith ((expected parser) "expression"))
  end
end
and parse_var_or_ctor_app = begin fun parser ->
  begin fun mod_names ->
    begin match parser.token with
      | ((Token.LowId(_)) | (Token.Reserved("$"))) ->
        begin let val_name = (parse_val_name parser) in
        (Expr.Var ((List.rev mod_names), val_name))
        end
      | (Token.CapId(_)) ->
        begin let capid = (parse_capid parser) in
        begin if ((( <> ) parser.token) (Token.Reserved ("."))) then
          ((parse_ctor_app parser) (Expr.Ctor ((List.rev mod_names), (Names.Id (capid)))))
        else
          begin
          (lookahead parser);
          ((parse_var_or_ctor_app parser) (( :: ) (capid, mod_names)))
          end
        end
        end
      | _ ->
        (failwith ((expected parser) "identifier"))
    end
  end
end
and parse_ctor_app = begin fun parser ->
  begin fun ctor ->
    begin match parser.token with
      | (Token.Reserved("(")) ->
        begin
        (lookahead parser);
        begin let arg_exprs = (parse_args parser) in
        (Expr.App (ctor, (Expr.Tuple (arg_exprs))))
        end
        end
      | (Token.Reserved("^")) ->
        begin let arg_expr = (parse_abs parser) in
        (Expr.App (ctor, arg_expr))
        end
      | _ ->
        ctor
    end
  end
end
and parse_val_path = begin fun parser ->
  begin fun mod_names ->
    begin match parser.token with
      | ((Token.LowId(_)) | (Token.Reserved("$"))) ->
        begin let val_name = (parse_val_name parser) in
        ((List.rev mod_names), val_name)
        end
      | (Token.CapId(_)) ->
        begin let capid = (parse_capid parser) in
        begin if ((( <> ) parser.token) (Token.Reserved ("."))) then
          (failwith ((expected parser) "'.'"))
        else
          begin
          (lookahead parser);
          ((parse_val_path parser) (( :: ) (capid, mod_names)))
          end
        end
        end
      | _ ->
        (failwith ((expected parser) "identifier"))
    end
  end
end
and parse_ctor = begin fun parser ->
  begin fun mod_names ->
    begin match parser.token with
      | (Token.CapId(_)) ->
        begin let capid = (parse_capid parser) in
        begin if ((( <> ) parser.token) (Token.Reserved ("."))) then
          ((List.rev mod_names), (Names.Id (capid)))
        else
          begin
          (lookahead parser);
          ((parse_ctor parser) (( :: ) (capid, mod_names)))
          end
        end
        end
      | _ ->
        (failwith ((expected parser) "capitalized identifier"))
    end
  end
end
and parse_mod_path = begin fun parser ->
  begin fun mod_names ->
    begin let capid = (parse_capid parser) in
    begin match parser.token with
      | (Token.Reserved(".")) ->
        begin
        (lookahead parser);
        ((parse_mod_path parser) (( :: ) (capid, mod_names)))
        end
      | _ ->
        (List.rev (( :: ) (capid, mod_names)))
    end
    end
  end
end
and parse_abs = begin fun parser ->
  begin
  (lookahead parser);
  begin let params = (parse_params parser) in
  begin let body_expr = (parse_block parser) in
  ((make_abs params) body_expr)
  end
  end
  end
end
and parse_params = begin fun parser ->
  begin if ((( <> ) parser.token) (Token.Reserved ("("))) then
    (failwith ((expected parser) "'('"))
  else
    begin
    (lookahead parser);
    begin if ((( = ) parser.token) (Token.Reserved (")"))) then
      begin
      (lookahead parser);
      (( :: ) ((Pattern.Variant (([], (Names.Id ("()"))), (( :: ) ((Pattern.Var ((Names.Id ("_")))), [])))), []))
      end
    else
      (parse_pattern_list parser)
    end
    end
  end
end
and parse_val_name = begin fun parser ->
  begin match parser.token with
    | (Token.LowId(_)) ->
      (Names.Id ((parse_lowid parser)))
    | (Token.Reserved("$")) ->
      begin
      (lookahead parser);
      begin if ((( <> ) parser.token) (Token.Reserved ("("))) then
        (failwith ((expected parser) "'('"))
      else
        begin
        (lookahead parser);
        (Names.Op ((parse_op parser)))
        end
      end
      end
    | _ ->
      (failwith ((expected parser) "identifier"))
  end
end
and parse_lowid = begin fun parser ->
  begin match parser.token with
    | (Token.LowId(str)) ->
      begin
      (lookahead parser);
      str
      end
    | _ ->
      (failwith ((expected parser) "lowercase identifier"))
  end
end
and parse_capid = begin fun parser ->
  begin match parser.token with
    | (Token.CapId(str)) ->
      begin
      (lookahead parser);
      str
      end
    | _ ->
      (failwith ((expected parser) "capitalized identifier"))
  end
end
and parse_op = begin fun parser ->
  begin match (Token.get_op parser.token) with
    | (Some(str)) ->
      begin
      (lookahead parser);
      begin if ((( <> ) parser.token) (Token.Reserved (")"))) then
        (failwith ((expected parser) "')'"))
      else
        begin
        (lookahead parser);
        str
        end
      end
      end
    | (None(_)) ->
      (failwith ((expected parser) "operator"))
  end
end
and parse_block = begin fun parser ->
  begin match parser.token with
    | (Token.Reserved(":")) ->
      begin
      (Lexer.indent parser.lexer);
      (parse_indented_block parser)
      end
    | (Token.Reserved("{")) ->
      (parse_braced_block parser)
    | _ ->
      (failwith ((expected parser) "':' or '{'"))
  end
end
and parse_indented_block = begin fun parser ->
  begin
  (lookahead parser);
  begin let expr = (parse_block_elem parser) in
  begin
  ((skip parser) (Token.Reserved (";")));
  begin if ((( <> ) parser.token) Token.Undent) then
    (failwith ((expected parser) "undent"))
  else
    begin
    (lookahead parser);
    expr
    end
  end
  end
  end
  end
end
and parse_braced_block = begin fun parser ->
  begin
  (lookahead parser);
  begin let expr = (parse_block_elem parser) in
  begin
  ((skip parser) (Token.Reserved (";")));
  begin if ((( <> ) parser.token) (Token.Reserved ("}"))) then
    (failwith ((expected parser) "'}'"))
  else
    begin
    (lookahead parser);
    expr
    end
  end
  end
  end
  end
end
and parse_block_elem = begin fun parser ->
  begin match parser.token with
    | (Token.Reserved("var")) ->
      (parse_let_val parser)
    | (Token.Reserved("def")) ->
      begin let defs = (( :: ) ((parse_let_fun parser), [])) in
      begin
      (parse_block_sep parser);
      begin let cont_expr = (parse_block_elem parser) in
      (Expr.LetFun (defs, cont_expr))
      end
      end
      end
    | (Token.Reserved("rec")) ->
      begin
      (lookahead parser);
      begin let defs = ((parse_block_like_elems parser) parse_let_fun) in
      begin
      (parse_block_sep parser);
      begin let cont_expr = (parse_block_elem parser) in
      (Expr.LetFun (defs, cont_expr))
      end
      end
      end
      end
    | _ ->
      begin let lhs = (parse_expr parser) in
      begin match parser.token with
        | (Token.Reserved(";")) ->
          begin
          (lookahead parser);
          begin
          ((skip parser) Token.Newline);
          begin let rhs = (parse_block_elem parser) in
          (Expr.Seq (lhs, rhs))
          end
          end
          end
        | (Token.Newline(_)) ->
          begin
          (lookahead parser);
          begin let rhs = (parse_block_elem parser) in
          (Expr.Seq (lhs, rhs))
          end
          end
        | _ ->
          lhs
      end
      end
  end
end
and parse_let_val = begin fun parser ->
  begin
  (lookahead parser);
  begin let val_pat = (parse_pattern parser) in
  begin if ((( <> ) parser.token) (Token.CmpOp ("="))) then
    (failwith ((expected parser) "'='"))
  else
    begin
    (lookahead parser);
    begin let val_expr = (parse_expr parser) in
    begin
    (parse_block_sep parser);
    begin let cont_expr = (parse_block_elem parser) in
    (Expr.LetVal (val_pat, val_expr, cont_expr))
    end
    end
    end
    end
  end
  end
  end
end
and parse_let_fun = begin fun parser ->
  begin if ((( <> ) parser.token) (Token.Reserved ("def"))) then
    (failwith ((expected parser) "'def'"))
  else
    begin
    (lookahead parser);
    begin let fun_name = (parse_val_name parser) in
    begin let params = (parse_params parser) in
    begin let body_expr = (parse_block parser) in
    (fun_name, ((make_abs params) body_expr))
    end
    end
    end
    end
  end
end
and parse_block_sep = begin fun parser ->
  begin match parser.token with
    | (Token.Reserved(";")) ->
      begin
      (lookahead parser);
      ((skip parser) Token.Newline)
      end
    | (Token.Newline(_)) ->
      (lookahead parser)
    | _ ->
      (failwith ((expected parser) "';' or newline"))
  end
end
and parse_if_expr = begin fun parser ->
  begin
  (lookahead parser);
  begin if ((( <> ) parser.token) (Token.Reserved ("("))) then
    (failwith ((expected parser) "'('"))
  else
    begin
    (lookahead parser);
    begin let cond_expr = (parse_expr parser) in
    begin if ((( <> ) parser.token) (Token.Reserved (")"))) then
      (failwith ((expected parser) "')'"))
    else
      begin
      (lookahead parser);
      begin let then_expr = (parse_block parser) in
      begin
      ((skip parser) Token.Newline);
      begin if ((( <> ) parser.token) (Token.Reserved ("else"))) then
        (failwith ((expected parser) "'else'"))
      else
        begin
        (lookahead parser);
        begin let else_expr = (parse_block parser) in
        (Expr.If (cond_expr, then_expr, else_expr))
        end
        end
      end
      end
      end
      end
    end
    end
    end
  end
  end
end
and parse_list = begin fun parser ->
  begin
  (lookahead parser);
  begin if ((( = ) parser.token) (Token.Reserved ("]"))) then
    begin
    (lookahead parser);
    (Expr.Var ([], ((Names.Id ("[]")))))
    end
  else
    begin let ctor = (Expr.Var ([], ((Names.Op ("::"))))) in
    begin let rec loop = begin fun expr ->
      begin if ((( = ) parser.token) (Token.Reserved ("]"))) then
        begin
        (lookahead parser);
        expr
        end
      else
        begin let elem = (parse_expr parser) in
        begin match parser.token with
          | (Token.Reserved("]")) ->
            begin
            (lookahead parser);
            (Expr.App (ctor, (Expr.Tuple ((( :: ) (elem, (( :: ) (expr, []))))))))
            end
          | (Token.Reserved(";")) ->
            begin
            (lookahead parser);
            (Expr.App (ctor, (Expr.Tuple ((( :: ) (elem, (( :: ) ((loop expr), []))))))))
            end
          | _ ->
            (failwith ((expected parser) "']' or ';'"))
        end
        end
      end
    end in
    (loop (Expr.Var ([], ((Names.Id ("[]"))))))
    end
    end
  end
  end
end
and parse_parens = begin fun parser ->
  begin
  (lookahead parser);
  begin if ((( = ) parser.token) (Token.Reserved (")"))) then
    begin
    (lookahead parser);
    (Expr.Var ([], ((Names.Id ("()")))))
    end
  else
    begin let list = (parse_expr_list parser) in
    (Expr.Tuple (list))
    end
  end
  end
end
and parse_args = begin fun parser ->
  begin if ((( = ) parser.token) (Token.Reserved (")"))) then
    begin
    (lookahead parser);
    (( :: ) ((Expr.Var ([], (Names.Id ("()")))), []))
    end
  else
    (parse_expr_list parser)
  end
end
and parse_expr_list = begin fun parser ->
  begin let rec sep_or_term = begin fun token ->
    begin match token with
      | (Token.Reserved(",")) ->
        Sep
      | (Token.Reserved(")")) ->
        Term
      | _ ->
        Neither
    end
  end in
  (((parse_elems parser) sep_or_term) parse_expr)
  end
end
and parse_record = begin fun parser ->
  begin
  (lookahead parser);
  (Expr.Record (((parse_braced_elems parser) parse_field_def)))
  end
end
and parse_field_def = begin fun parser ->
  begin let field_name = ((parse_val_path parser) []) in
  begin if ((( <> ) parser.token) (Token.CmpOp ("="))) then
    (failwith ((expected parser) "'='"))
  else
    begin
    (lookahead parser);
    begin let expr = (parse_expr parser) in
    (field_name, expr)
    end
    end
  end
  end
end
and parse_match_expr = begin fun parser ->
  begin
  (lookahead parser);
  begin if ((( <> ) parser.token) (Token.Reserved ("("))) then
    (failwith ((expected parser) "'('"))
  else
    begin
    (lookahead parser);
    begin let target_expr = (parse_expr parser) in
    begin if ((( <> ) parser.token) (Token.Reserved (")"))) then
      (failwith ((expected parser) "')'"))
    else
      begin
      (lookahead parser);
      begin let cases = ((parse_block_like_elems parser) parse_case) in
      (Expr.Match (target_expr, cases))
      end
      end
    end
    end
    end
  end
  end
end
and parse_case = begin fun parser ->
  begin match parser.token with
    | (Token.Reserved("case")) ->
      begin
      (lookahead parser);
      begin let pat = (parse_pattern parser) in
      begin if ((( = ) parser.token) (Token.Reserved ("when"))) then
        begin
        (lookahead parser);
        begin let guard = (parse_expr parser) in
        begin let expr = (parse_block parser) in
        (pat, (Some (guard)), expr)
        end
        end
        end
      else
        begin let expr = (parse_block parser) in
        (pat, None, expr)
        end
      end
      end
      end
    | _ ->
      (failwith ((expected parser) "'case'"))
  end
end
and parse_pattern = begin fun parser ->
  (parse_or_pattern parser)
end
and parse_or_pattern = begin fun parser ->
  begin let lhs = (parse_cons_pattern parser) in
  begin let rec loop = begin fun lhs ->
    begin match parser.token with
      | (Token.CmpOp("|")) ->
        begin
        (lookahead parser);
        begin let rhs = (parse_cons_pattern parser) in
        (loop (Pattern.Or (lhs, rhs)))
        end
        end
      | _ ->
        lhs
    end
  end in
  (loop lhs)
  end
  end
end
and parse_cons_pattern = begin fun parser ->
  begin let lhs = (parse_atomic_pattern parser) in
  begin match parser.token with
    | (Token.ConsOp("::")) ->
      begin
      (lookahead parser);
      begin let rhs = (parse_cons_pattern parser) in
      (Pattern.Variant (([], (Names.Op ("::"))), (( :: ) (lhs, (( :: ) (rhs, []))))))
      end
      end
    | _ ->
      lhs
  end
  end
end
and parse_atomic_pattern = begin fun parser ->
  begin match parser.token with
    | (((Token.Int(_)) | (Token.String(_))) | (Token.Char(_))) ->
      begin let lit = (parse_literal parser) in
      (Pattern.Con (lit))
      end
    | (Token.LowId(_)) ->
      begin let name = (parse_val_name parser) in
      (Pattern.Var (name))
      end
    | (Token.CapId(_)) ->
      (parse_variant_pattern parser)
    | (Token.Reserved("[")) ->
      (parse_list_pattern parser)
    | (Token.Reserved("{")) ->
      (parse_record_pattern parser)
    | (Token.Reserved("(")) ->
      (parse_parens_pattern parser)
    | _ ->
      (failwith ((expected parser) "pattern"))
  end
end
and parse_variant_pattern = begin fun parser ->
  begin let ctor = ((parse_ctor parser) []) in
  begin if ((( <> ) parser.token) (Token.Reserved ("("))) then
    (Pattern.Variant (ctor, (( :: ) ((Pattern.Var ((Names.Id ("_")))), []))))
  else
    begin
    (lookahead parser);
    begin let pat_list = (parse_pattern_list parser) in
    (Pattern.Variant (ctor, pat_list))
    end
    end
  end
  end
end
and parse_list_pattern = begin fun parser ->
  begin
  (lookahead parser);
  begin if ((( = ) parser.token) (Token.Reserved ("]"))) then
    begin
    (lookahead parser);
    (Pattern.Variant (([], (Names.Id ("[]"))), (( :: ) ((Pattern.Var ((Names.Id ("_")))), []))))
    end
  else
    begin let rec sep_or_term = begin fun token ->
      begin match token with
        | (Token.Reserved(";")) ->
          Sep
        | (Token.Reserved("]")) ->
          Term
        | _ ->
          Neither
      end
    end in
    begin let list = (((parse_elems parser) sep_or_term) parse_pattern) in
    (((List.fold_right begin fun elem ->
      begin fun acc ->
        (Pattern.Variant (([], (Names.Op ("::"))), (( :: ) (elem, (( :: ) (acc, []))))))
      end
    end) list) (Pattern.Variant (([], (Names.Id ("[]"))), (( :: ) ((Pattern.Var ((Names.Id ("_")))), [])))))
    end
    end
  end
  end
end
and parse_record_pattern = begin fun parser ->
  begin
  (lookahead parser);
  (Pattern.Record (((parse_braced_elems parser) parse_field_pattern)))
  end
end
and parse_field_pattern = begin fun parser ->
  begin let field_name = ((parse_val_path parser) []) in
  begin if ((( <> ) parser.token) (Token.Reserved ("="))) then
    (field_name, None)
  else
    begin
    (lookahead parser);
    begin let pattern = (parse_pattern parser) in
    (field_name, (Some (pattern)))
    end
    end
  end
  end
end
and parse_parens_pattern = begin fun parser ->
  begin
  (lookahead parser);
  begin if ((( = ) parser.token) (Token.Reserved (")"))) then
    begin
    (lookahead parser);
    (Pattern.Variant (([], (Names.Id ("()"))), (( :: ) ((Pattern.Var ((Names.Id ("_")))), []))))
    end
  else
    begin let list = (parse_pattern_list parser) in
    (Pattern.Tuple (list))
    end
  end
  end
end
and parse_pattern_list = begin fun parser ->
  begin let rec sep_or_term = begin fun token ->
    begin match token with
      | (Token.Reserved(",")) ->
        Sep
      | (Token.Reserved(")")) ->
        Term
      | _ ->
        Neither
    end
  end in
  (((parse_elems parser) sep_or_term) parse_pattern)
  end
end
and parse_literal = begin fun parser ->
  begin match parser.token with
    | (Token.Int(n)) ->
      begin
      (lookahead parser);
      (Literal.Int (n))
      end
    | (Token.String(str)) ->
      begin
      (lookahead parser);
      (Literal.String (str))
      end
    | (Token.Char(str)) ->
      begin
      (lookahead parser);
      (Literal.Char (str))
      end
    | _ ->
      (failwith ((expected parser) "literal"))
  end
end
and parse_stmt = begin fun parser ->
  begin let expr = (parse_top parser) in
  begin match parser.token with
    | (((Token.EOF(_)) | (Token.Newline(_))) | (Token.Reserved(";"))) ->
      expr
    | _ ->
      (failwith ((expected parser) "newline or ';'"))
  end
  end
end
and parse = begin fun parser ->
  begin
  (lookahead parser);
  begin
  ((skip parser) Token.Newline);
  begin match parser.token with
    | (Token.EOF(_)) ->
      None
    | _ ->
      (Some ((parse_stmt parser)))
  end
  end
  end
end

