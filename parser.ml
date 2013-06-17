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

let rec make_cons_pattern = begin fun lhs ->
  begin fun rhs ->
    (Pattern.Variant (([], (Names.Op ("::"))), (( :: ) (lhs, (( :: ) (rhs, []))))))
  end
end

let wildcard_pattern = (Pattern.Var ((Names.Id ("_"))))

let nil_pattern = (Pattern.Variant (([], (Names.Id ("[]"))), (( :: ) (wildcard_pattern, []))))

let unit_pattern = (Pattern.Variant (([], (Names.Id ("()"))), (( :: ) (wildcard_pattern, []))))

let rec make_op_var = begin fun str ->
  (Expr.Var ([], (Names.Op (str))))
end

let nil_expr = (Expr.Var ([], ((Names.Id ("[]")))))

let unit_expr = (Expr.Var ([], ((Names.Id ("()")))))

let rec make_abs = begin fun params ->
  begin fun body_expr ->
    (((YzList.fold_right params) body_expr) begin fun param ->
      begin fun expr ->
        (Expr.Abs (param, expr))
      end
    end)
  end
end

let rec make_app = begin fun fun_expr ->
  begin fun arg_exprs ->
    (((YzList.fold_left fun_expr) arg_exprs) begin fun e1 ->
      begin fun e2 ->
        (Expr.App (e1, e2))
      end
    end)
  end
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

let rec parse_token = begin fun parser ->
  begin fun token ->
    begin if ((( <> ) parser.token) token) then
      (failwith ((expected parser) (Token.show token)))
    else
      (lookahead parser)
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
          begin let op = (make_op_var str) in
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
          begin let op = (make_op_var str) in
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
            begin let op = (make_op_var str) in
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

let rec comma_or_rparen = begin fun token ->
  begin match token with
    | (Token.Reserved(",")) ->
      Sep
    | (Token.Reserved(")")) ->
      Term
    | _ ->
      Neither
  end
end

let rec semi_or_rbracket = begin fun token ->
  begin match token with
    | (Token.Reserved(";")) ->
      Sep
    | (Token.Reserved("]")) ->
      Term
    | _ ->
      Neither
  end
end

let rec semi_or_newline_or_undent = begin fun token ->
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
end

let rec semi_or_rbrace = begin fun token ->
  begin match token with
    | (Token.Reserved(";")) ->
      Sep
    | (Token.Reserved("}")) ->
      Term
    | _ ->
      Neither
  end
end

let rec parse_indented_elems = begin fun parser ->
  begin fun parse_elem ->
    (((parse_elems parser) semi_or_newline_or_undent) parse_elem)
  end
end

let rec parse_braced_elems = begin fun parser ->
  begin fun parse_elem ->
    (((parse_elems parser) semi_or_rbrace) parse_elem)
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

let rec parse_literal = begin fun parser ->
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

let rec parse_capid = begin fun parser ->
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

let rec parse_lowid = begin fun parser ->
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

let rec parse_op = begin fun parser ->
  begin
  ((parse_token parser) (Token.Reserved ("(")));
  begin match (Token.get_op parser.token) with
    | (Some(str)) ->
      begin
      (lookahead parser);
      begin
      ((parse_token parser) (Token.Reserved (")")));
      str
      end
      end
    | (None(_)) ->
      (failwith ((expected parser) "operator"))
  end
  end
end

let rec parse_val_name = begin fun parser ->
  begin match parser.token with
    | (Token.LowId(_)) ->
      (Names.Id ((parse_lowid parser)))
    | (Token.Reserved("$")) ->
      begin
      (lookahead parser);
      (Names.Op ((parse_op parser)))
      end
    | _ ->
      (failwith ((expected parser) "identifier"))
  end
end

let rec parse_val_path = begin fun parser ->
  begin fun mod_names ->
    begin match parser.token with
      | ((Token.LowId(_)) | (Token.Reserved("$"))) ->
        begin let val_name = (parse_val_name parser) in
        ((List.rev mod_names), val_name)
        end
      | (Token.CapId(_)) ->
        begin let capid = (parse_capid parser) in
        begin
        ((parse_token parser) (Token.Reserved (".")));
        ((parse_val_path parser) (( :: ) (capid, mod_names)))
        end
        end
      | _ ->
        (failwith ((expected parser) "identifier"))
    end
  end
end

let rec parse_ctor = begin fun parser ->
  begin fun mod_names ->
    begin match parser.token with
      | (Token.CapId(_)) ->
        begin let capid = (parse_capid parser) in
        begin if ((( = ) parser.token) (Token.Reserved ("."))) then
          begin
          (lookahead parser);
          ((parse_ctor parser) (( :: ) (capid, mod_names)))
          end
        else
          ((List.rev mod_names), (Names.Id (capid)))
        end
        end
      | _ ->
        (failwith ((expected parser) "capitalized identifier"))
    end
  end
end

let rec parse_mod_path = begin fun parser ->
  begin fun mod_names ->
    begin let capid = (parse_capid parser) in
    begin if ((( = ) parser.token) (Token.Reserved ("."))) then
      begin
      (lookahead parser);
      ((parse_mod_path parser) (( :: ) (capid, mod_names)))
      end
    else
      (List.rev (( :: ) (capid, mod_names)))
    end
    end
  end
end

let rec parse_type = begin fun parser ->
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
      (TypeExpr.Tuple ((loop (( :: ) (t, [])))))
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
      begin if ((( = ) parser.token) (Token.Reserved ("("))) then
        begin
        (lookahead parser);
        begin let args = (((parse_elems parser) comma_or_rparen) parse_type) in
        (TypeExpr.App (typector, args))
        end
        end
      else
        (TypeExpr.Con (typector))
      end
      end
    | (Token.Reserved("(")) ->
      begin
      (lookahead parser);
      begin let t = (parse_type parser) in
      begin
      ((parse_token parser) (Token.Reserved (")")));
      t
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
        begin
        ((parse_token parser) (Token.Reserved (".")));
        ((parse_typector parser) (( :: ) (capid, mod_names)))
        end
        end
      | _ ->
        (failwith ((expected parser) "identifier"))
    end
  end
end

let rec parse_pattern = begin fun parser ->
  (parse_as_pattern parser)
end

and parse_as_pattern = begin fun parser ->
  begin let pat = (parse_or_pattern parser) in
  begin if ((( = ) parser.token) (Token.Reserved ("as"))) then
    begin
    (lookahead parser);
    begin let name = (parse_val_name parser) in
    (Pattern.As (pat, name))
    end
    end
  else
    pat
  end
  end
end

and parse_or_pattern = begin fun parser ->
  begin let lhs = (parse_cons_pattern parser) in
  begin let rec loop = begin fun lhs ->
    begin if ((( = ) parser.token) (Token.CmpOp ("|"))) then
      begin
      (lookahead parser);
      begin let rhs = (parse_cons_pattern parser) in
      (loop (Pattern.Or (lhs, rhs)))
      end
      end
    else
      lhs
    end
  end in
  (loop lhs)
  end
  end
end

and parse_cons_pattern = begin fun parser ->
  begin let lhs = (parse_atomic_pattern parser) in
  begin if ((( = ) parser.token) (Token.ConsOp ("::"))) then
    begin
    (lookahead parser);
    begin let rhs = (parse_cons_pattern parser) in
    ((make_cons_pattern lhs) rhs)
    end
    end
  else
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
      begin
      (lookahead parser);
      (parse_list_pattern parser)
      end
    | (Token.Reserved("{")) ->
      begin
      (lookahead parser);
      (parse_record_pattern parser)
      end
    | (Token.Reserved("(")) ->
      begin
      (lookahead parser);
      (parse_parens_pattern parser)
      end
    | _ ->
      (failwith ((expected parser) "pattern"))
  end
end

and parse_variant_pattern = begin fun parser ->
  begin let ctor = ((parse_ctor parser) []) in
  begin if ((( = ) parser.token) (Token.Reserved ("("))) then
    begin
    (lookahead parser);
    begin let pat_list = (parse_pattern_list parser) in
    (Pattern.Variant (ctor, pat_list))
    end
    end
  else
    (Pattern.Variant (ctor, (( :: ) (wildcard_pattern, []))))
  end
  end
end

and parse_list_pattern = begin fun parser ->
  begin let list = (((parse_elems parser) semi_or_rbracket) parse_pattern) in
  (((YzList.fold_right list) nil_pattern) make_cons_pattern)
  end
end

and parse_record_pattern = begin fun parser ->
  (Pattern.Record (((parse_braced_elems parser) parse_field_pattern)))
end

and parse_field_pattern = begin fun parser ->
  begin let field_name = ((parse_val_path parser) []) in
  begin if ((( = ) parser.token) (Token.Reserved ("="))) then
    begin
    (lookahead parser);
    begin let pattern = (parse_pattern parser) in
    (field_name, (Some (pattern)))
    end
    end
  else
    (field_name, None)
  end
  end
end

and parse_parens_pattern = begin fun parser ->
  begin if ((( = ) parser.token) (Token.Reserved (")"))) then
    begin
    (lookahead parser);
    unit_pattern
    end
  else
    begin let list = (parse_pattern_list parser) in
    (Pattern.Tuple (list))
    end
  end
end

and parse_pattern_list = begin fun parser ->
  (((parse_elems parser) comma_or_rparen) parse_pattern)
end

let rec parse_expr = begin fun parser ->
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
  begin let lhs = (parse_and_expr parser) in
  begin match parser.token with
    | (Token.OrOp("||")) ->
      begin
      (lookahead parser);
      begin let rhs = (parse_or_expr parser) in
      (Expr.Or (lhs, rhs))
      end
      end
    | _ ->
      lhs
  end
  end
end

and parse_and_expr = begin fun parser ->
  begin let lhs = (parse_cmp_expr parser) in
  begin match parser.token with
    | (Token.AndOp("&&")) ->
      begin
      (lookahead parser);
      begin let rhs = (parse_and_expr parser) in
      (Expr.And (lhs, rhs))
      end
      end
    | _ ->
      lhs
  end
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
      begin let op = (make_op_var str) in
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
      (Expr.App ((make_op_var "~-"), expr))
      end
      end
    | (Token.AddOp("+")) ->
      begin
      (lookahead parser);
      begin let expr = (parse_unary_expr parser) in
      (Expr.App ((make_op_var "~+"), expr))
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
        begin
        (lookahead parser);
        begin let arg_expr = (parse_abs parser) in
        (loop (Expr.App (fun_expr, arg_expr)))
        end
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
      begin match parser.token with
        | (Token.Reserved("{")) ->
          begin
          (lookahead parser);
          (Expr.Update (expr, ((parse_braced_elems parser) parse_field_def)))
          end
        | _ ->
          begin let path = ((parse_val_path parser) []) in
          (Expr.Field (expr, path))
          end
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
      begin
      (lookahead parser);
      (parse_abs parser)
      end
    | (Token.Reserved("[")) ->
      begin
      (lookahead parser);
      (parse_list parser)
      end
    | (Token.Reserved("(")) ->
      begin
      (lookahead parser);
      (parse_parens parser)
      end
    | (Token.Reserved("{")) ->
      begin
      (lookahead parser);
      (parse_record parser)
      end
    | (Token.Reserved("if")) ->
      begin
      (lookahead parser);
      (parse_if_expr parser)
      end
    | (Token.Reserved("match")) ->
      begin
      (lookahead parser);
      (parse_match_expr parser)
      end
    | (Token.Reserved("try")) ->
      begin
      (lookahead parser);
      (parse_try_expr parser)
      end
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
        begin if ((( = ) parser.token) (Token.Reserved ("."))) then
          begin
          (lookahead parser);
          ((parse_var_or_ctor_app parser) (( :: ) (capid, mod_names)))
          end
        else
          ((parse_ctor_app parser) (Expr.Var ((List.rev mod_names), (Names.Id (capid)))))
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
        begin
        (lookahead parser);
        begin let arg_expr = (parse_abs parser) in
        (Expr.App (ctor, arg_expr))
        end
        end
      | _ ->
        ctor
    end
  end
end

and parse_abs = begin fun parser ->
  begin let params = (parse_params parser) in
  begin let body_expr = (parse_block parser) in
  ((make_abs params) body_expr)
  end
  end
end

and parse_params = begin fun parser ->
  begin
  ((parse_token parser) (Token.Reserved ("(")));
  begin if ((( = ) parser.token) (Token.Reserved (")"))) then
    begin
    (lookahead parser);
    (( :: ) (unit_pattern, []))
    end
  else
    (parse_pattern_list parser)
  end
  end
end

and parse_block = begin fun parser ->
  begin match parser.token with
    | (Token.Reserved(":")) ->
      begin
      (Lexer.indent parser.lexer);
      begin
      (lookahead parser);
      (parse_indented_block parser)
      end
      end
    | (Token.Reserved("{")) ->
      begin
      (lookahead parser);
      (parse_braced_block parser)
      end
    | _ ->
      (failwith ((expected parser) "':' or '{'"))
  end
end

and parse_indented_block = begin fun parser ->
  begin let expr = ((parse_block_elem parser) semi_or_newline_or_undent) in
  begin
  ((parse_token parser) Token.Undent);
  expr
  end
  end
end

and parse_braced_block = begin fun parser ->
  begin let expr = ((parse_block_elem parser) semi_or_rbrace) in
  begin
  ((parse_token parser) (Token.Reserved ("}")));
  expr
  end
  end
end

and parse_block_elem = begin fun parser ->
  begin fun sep_or_term ->
    begin match parser.token with
      | (Token.Reserved("var")) ->
        begin
        (lookahead parser);
        begin let (val_name, val_expr) = (parse_let_val parser) in
        begin
        ((parse_sep parser) sep_or_term);
        begin let cont_expr = ((parse_block_elem parser) sep_or_term) in
        (Expr.LetVal (val_name, val_expr, cont_expr))
        end
        end
        end
        end
      | (Token.Reserved("def")) ->
        begin let defs = (( :: ) ((parse_let_fun parser), [])) in
        begin
        ((parse_sep parser) sep_or_term);
        begin let cont_expr = ((parse_block_elem parser) sep_or_term) in
        (Expr.LetFun (defs, cont_expr))
        end
        end
        end
      | (Token.Reserved("rec")) ->
        begin
        (lookahead parser);
        begin let defs = ((parse_block_like_elems parser) parse_let_fun) in
        begin
        ((parse_sep parser) sep_or_term);
        begin let cont_expr = ((parse_block_elem parser) sep_or_term) in
        (Expr.LetFun (defs, cont_expr))
        end
        end
        end
        end
      | _ ->
        begin let lhs = (parse_expr parser) in
        begin match (sep_or_term parser.token) with
          | (Sep(_)) ->
            begin
            (lookahead parser);
            begin match (sep_or_term parser.token) with
              | (Term(_)) ->
                lhs
              | _ ->
                begin let rhs = ((parse_block_elem parser) sep_or_term) in
                (Expr.Seq (lhs, rhs))
                end
            end
            end
          | _ ->
            lhs
        end
        end
    end
  end
end

and parse_let_val = begin fun parser ->
  begin let val_pat = (parse_pattern parser) in
  begin
  ((parse_token parser) (Token.CmpOp ("=")));
  begin let val_expr = (parse_expr parser) in
  (val_pat, val_expr)
  end
  end
  end
end

and parse_let_fun = begin fun parser ->
  begin
  ((parse_token parser) (Token.Reserved ("def")));
  begin let fun_name = (parse_val_name parser) in
  begin let params = (parse_params parser) in
  begin let body_expr = (parse_block parser) in
  (fun_name, ((make_abs params) body_expr))
  end
  end
  end
  end
end

and parse_sep = begin fun parser ->
  begin fun sep_or_term ->
    begin match (sep_or_term parser.token) with
      | (Sep(_)) ->
        (lookahead parser)
      | _ ->
        (failwith ((expected parser) "separator"))
    end
  end
end

and parse_list = begin fun parser ->
  begin let list = (((parse_elems parser) semi_or_rbracket) parse_expr) in
  begin let ctor = (make_op_var "::") in
  (((YzList.fold_right list) nil_expr) begin fun elem ->
    begin fun acc ->
      (Expr.App (ctor, (Expr.Tuple ((( :: ) (elem, (( :: ) (acc, []))))))))
    end
  end)
  end
  end
end

and parse_parens = begin fun parser ->
  begin if ((( = ) parser.token) (Token.Reserved (")"))) then
    begin
    (lookahead parser);
    unit_expr
    end
  else
    begin let list = (parse_expr_list parser) in
    (Expr.Tuple (list))
    end
  end
end

and parse_args = begin fun parser ->
  begin if ((( = ) parser.token) (Token.Reserved (")"))) then
    begin
    (lookahead parser);
    (( :: ) (unit_expr, []))
    end
  else
    (parse_expr_list parser)
  end
end

and parse_expr_list = begin fun parser ->
  (((parse_elems parser) comma_or_rparen) parse_expr)
end

and parse_record = begin fun parser ->
  (Expr.Record (((parse_braced_elems parser) parse_field_def)))
end

and parse_field_def = begin fun parser ->
  begin let field_name = ((parse_val_path parser) []) in
  begin
  ((parse_token parser) (Token.CmpOp ("=")));
  begin let expr = (parse_expr parser) in
  (field_name, expr)
  end
  end
  end
end

and parse_if_expr = begin fun parser ->
  begin let cond_expr = (parse_expr parser) in
  begin let then_expr = (parse_block parser) in
  begin
  ((skip parser) Token.Newline);
  begin
  ((parse_token parser) (Token.Reserved ("else")));
  begin let else_expr = (parse_block parser) in
  (Expr.If (cond_expr, then_expr, else_expr))
  end
  end
  end
  end
  end
end

and parse_match_expr = begin fun parser ->
  begin let target_expr = (parse_expr parser) in
  begin let cases = ((parse_block_like_elems parser) parse_case) in
  (Expr.Match (target_expr, cases))
  end
  end
end

and parse_try_expr = begin fun parser ->
  begin let expr = (parse_block parser) in
  begin
  ((skip parser) Token.Newline);
  begin
  ((parse_token parser) (Token.Reserved ("with")));
  begin let cases = ((parse_block_like_elems parser) parse_case) in
  (Expr.Try (expr, cases))
  end
  end
  end
  end
end

and parse_case = begin fun parser ->
  begin
  ((parse_token parser) (Token.Reserved ("case")));
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
end

let rec parse_top = begin fun parser ->
  begin match parser.token with
    | (Token.Reserved("var")) ->
      begin
      (lookahead parser);
      (parse_top_let_val parser)
      end
    | (Token.Reserved("def")) ->
      (Top.LetFun ((( :: ) ((parse_top_let_fun parser), []))))
    | (Token.Reserved("rec")) ->
      begin
      (lookahead parser);
      (Top.LetFun (((parse_block_like_elems parser) parse_top_let_fun)))
      end
    | (Token.Reserved("open")) ->
      begin
      (lookahead parser);
      (parse_top_open parser)
      end
    | (Token.Reserved("exception")) ->
      begin
      (lookahead parser);
      (parse_top_exn_decl parser)
      end
    | (Token.Reserved("type")) ->
      begin
      (lookahead parser);
      (parse_top_typedef parser)
      end
    | _ ->
      (Top.Expr ((parse_expr parser)))
  end
end

and parse_top_let_val = begin fun parser ->
  begin let val_pat = (parse_pattern parser) in
  begin
  ((parse_token parser) (Token.CmpOp ("=")));
  (Top.LetVal (val_pat, (parse_expr parser)))
  end
  end
end

and parse_top_let_fun = begin fun parser ->
  begin
  ((parse_token parser) (Token.Reserved ("def")));
  begin let fun_name = (parse_val_name parser) in
  begin let params = (parse_params parser) in
  begin let body_expr = (parse_block parser) in
  (fun_name, ((make_abs params) body_expr))
  end
  end
  end
  end
end

and parse_top_open = begin fun parser ->
  begin let mod_path = ((parse_mod_path parser) []) in
  (Top.Open (mod_path))
  end
end

and parse_top_exn_decl = begin fun parser ->
  begin let exn_name = (Names.Id ((parse_capid parser))) in
  begin if ((( = ) parser.token) (Token.Reserved ("("))) then
    begin
    (lookahead parser);
    begin let t = (parse_type parser) in
    begin
    ((parse_token parser) (Token.Reserved (")")));
    (Top.Exception (exn_name, (Some (t))))
    end
    end
    end
  else
    (Top.Exception (exn_name, None))
  end
  end
end

and parse_top_typedef = begin fun parser ->
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
  ((parse_token parser) (Token.Reserved ("def")));
  begin let ctor_name = (Names.Id ((parse_capid parser))) in
  begin if ((( = ) parser.token) (Token.Reserved ("("))) then
    begin
    (lookahead parser);
    begin let t = (parse_type parser) in
    begin
    ((parse_token parser) (Token.Reserved (")")));
    (ctor_name, (Some (t)))
    end
    end
    end
  else
    (ctor_name, None)
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
  begin
  ((parse_token parser) (Token.Reserved (":")));
  begin let t = (parse_type parser) in
  (is_mutable, field_name, t)
  end
  end
  end
  end
  end
end

let rec parse_stmt = begin fun parser ->
  begin let expr = (parse_top parser) in
  begin match parser.token with
    | (((Token.EOF(_)) | (Token.Newline(_))) | (Token.Reserved(";"))) ->
      expr
    | _ ->
      (failwith ((expected parser) "newline or ';'"))
  end
  end
end

let rec parse = begin fun parser ->
  begin
  (lookahead parser);
  begin match parser.token with
    | (Token.EOF(_)) ->
      None
    | _ ->
      (Some ((parse_stmt parser)))
  end
  end
end

