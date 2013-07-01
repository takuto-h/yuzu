open YzPervasives

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

let rec make_cons_pattern = begin fun pos ->
  begin fun lhs ->
    begin fun rhs ->
      begin let tuple_pattern = ((Pattern.at pos) (Pattern.Tuple (( :: ) (lhs, (( :: ) (rhs, ( [] ))))))) in
      ((Pattern.at pos) (Pattern.Ctor ((( [] ), (Names.Op "::")), (Some tuple_pattern))))
      end
    end
  end
end

let nil_pattern = (Pattern.Ctor ((( [] ), (Names.Op "[]")), None))

let unit_pattern = (Pattern.Con Literal.Unit)

let rec make_op_var = begin fun str ->
  (Expr.Var (( [] ), (Names.Op str)))
end

let cons_op = (( [] ), (Names.Op "::"))

let nil_expr = (Expr.Ctor ((( [] ), (Names.Op "[]")), None))

let unit_expr = (Expr.Con Literal.Unit)

let exn_type_expr = (TypeExpr.Con (( [] ), "exn"))

let rec make_abs = begin fun pos ->
  begin fun params ->
    begin fun body_expr ->
      begin let rec mk_abs = begin fun param ->
        begin fun expr ->
          ((Expr.at pos) (Expr.Abs (param, expr)))
        end
      end in
      (((List.fold_right mk_abs) params) body_expr)
      end
    end
  end
end

let rec make_app = begin fun pos ->
  begin fun fun_expr ->
    begin fun arg_exprs ->
      begin let rec mk_app = begin fun e1 ->
        begin fun e2 ->
          ((Expr.at pos) (Expr.App (e1, e2)))
        end
      end in
      (((List.fold_left mk_app) fun_expr) arg_exprs)
      end
    end
  end
end

let rec expected = begin fun parser ->
  begin fun str_token ->
    ((Pos.show_error parser.pos) (((sprintf "unexpected %s, expected %s\n") (Token.show parser.token)) str_token))
  end
end

let rec lookahead = begin fun parser ->
  begin match (Lexer.next parser.lexer) with
    | (None, pos) ->
      begin
      (parser.token <- Token.EOF);
      (parser.pos <- pos)
      end
    | ((Some token), pos) ->
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
        | None ->
          lhs
        | (Some str) ->
          begin let pos = parser.pos in
          begin
          (lookahead parser);
          begin let op = ((Expr.at pos) (make_op_var str)) in
          begin let rhs = (parse_lower parser) in
          ((Expr.at pos) (Expr.App (((Expr.at pos) (Expr.App (op, lhs))), rhs)))
          end
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
        | None ->
          lhs
        | (Some str) ->
          begin let pos = parser.pos in
          begin
          (lookahead parser);
          begin let op = ((Expr.at pos) (make_op_var str)) in
          begin let rhs = (((parse_right_assoc parser) get_op) parse_lower) in
          ((Expr.at pos) (Expr.App (((Expr.at pos) (Expr.App (op, lhs))), rhs)))
          end
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
          | None ->
            lhs
          | (Some str) ->
            begin let pos = parser.pos in
            begin
            (lookahead parser);
            begin let op = ((Expr.at pos) (make_op_var str)) in
            begin let rhs = (parse_lower parser) in
            (loop ((Expr.at pos) (Expr.App (((Expr.at pos) (Expr.App (op, lhs))), rhs))))
            end
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
          | Term ->
            begin
            (lookahead parser);
            (List.rev elems)
            end
          | _ ->
            begin let elem = (parse_elem parser) in
            begin match (sep_or_term parser.token) with
              | Term ->
                begin
                (lookahead parser);
                (List.rev (( :: ) (elem, elems)))
                end
              | Sep ->
                begin
                (lookahead parser);
                (loop (( :: ) (elem, elems)))
                end
              | Neither ->
                (failwith ((expected parser) "separator or terminator"))
            end
            end
        end
      end in
      (loop ( [] ))
      end
    end
  end
end

let rec comma_or_rparen = begin fun token ->
  begin match token with
    | (Token.Reserved ",") ->
      Sep
    | (Token.Reserved ")") ->
      Term
    | _ ->
      Neither
  end
end

let rec semi_or_rbracket = begin fun token ->
  begin match token with
    | (Token.Reserved ";") ->
      Sep
    | (Token.Reserved "]") ->
      Term
    | _ ->
      Neither
  end
end

let rec semi_or_newline_or_undent = begin fun token ->
  begin match token with
    | (Token.Reserved ";") ->
      Sep
    | Token.Newline ->
      Sep
    | Token.Undent ->
      Term
    | _ ->
      Neither
  end
end

let rec semi_or_rbrace = begin fun token ->
  begin match token with
    | (Token.Reserved ";") ->
      Sep
    | (Token.Reserved "}") ->
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
      | (Token.Reserved ":") ->
        begin
        (Lexer.indent parser.lexer);
        begin
        (lookahead parser);
        ((parse_indented_elems parser) parse_elem)
        end
        end
      | (Token.Reserved "{") ->
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
    | (Token.Int n) ->
      begin
      (lookahead parser);
      (Literal.Int n)
      end
    | (Token.String str) ->
      begin
      (lookahead parser);
      (Literal.String str)
      end
    | (Token.Char str) ->
      begin
      (lookahead parser);
      (Literal.Char str)
      end
    | (Token.Reserved "true") ->
      begin
      (lookahead parser);
      (Literal.Bool true)
      end
    | (Token.Reserved "false") ->
      begin
      (lookahead parser);
      (Literal.Bool false)
      end
    | _ ->
      (failwith ((expected parser) "literal"))
  end
end

let rec parse_capid = begin fun parser ->
  begin match parser.token with
    | (Token.CapId str) ->
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
    | (Token.LowId str) ->
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
  ((parse_token parser) (Token.Reserved "("));
  begin if ((( = ) parser.token) (Token.Reserved "[")) then
    begin
    (lookahead parser);
    begin
    ((parse_token parser) (Token.Reserved "]"));
    begin
    ((parse_token parser) (Token.Reserved ")"));
    "[]"
    end
    end
    end
  else
    begin match (Token.get_op parser.token) with
      | (Some str) ->
        begin
        (lookahead parser);
        begin
        ((parse_token parser) (Token.Reserved ")"));
        str
        end
        end
      | None ->
        (failwith ((expected parser) "operator"))
    end
  end
  end
end

let rec parse_val_name = begin fun parser ->
  begin match parser.token with
    | (Token.LowId _) ->
      (Names.Id (parse_lowid parser))
    | (Token.Reserved "$") ->
      begin
      (lookahead parser);
      (Names.Op (parse_op parser))
      end
    | _ ->
      (failwith ((expected parser) "identifier"))
  end
end

let rec parse_ctor_name = begin fun parser ->
  begin match parser.token with
    | (Token.CapId _) ->
      (Names.Id (parse_capid parser))
    | (Token.Reserved "$") ->
      begin
      (lookahead parser);
      (Names.Op (parse_op parser))
      end
    | _ ->
      (failwith ((expected parser) "identifier"))
  end
end

let rec parse_val_path = begin fun parser ->
  begin fun mod_names ->
    begin match parser.token with
      | ((Token.LowId _) | (Token.Reserved "$")) ->
        begin let val_name = (parse_val_name parser) in
        ((List.rev mod_names), val_name)
        end
      | (Token.CapId _) ->
        begin let capid = (parse_capid parser) in
        begin
        ((parse_token parser) (Token.Reserved "."));
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
      | (Token.Reserved "$") ->
        begin let ctor_name = (parse_ctor_name parser) in
        ((List.rev mod_names), ctor_name)
        end
      | (Token.CapId _) ->
        begin let capid = (parse_capid parser) in
        begin if ((( = ) parser.token) (Token.Reserved ".")) then
          begin
          (lookahead parser);
          ((parse_ctor parser) (( :: ) (capid, mod_names)))
          end
        else
          ((List.rev mod_names), (Names.Id capid))
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
    begin if ((( = ) parser.token) (Token.Reserved ".")) then
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
  (parse_fun_type parser)
end

and parse_fun_type = begin fun parser ->
  begin let lhs = (parse_tuple_type parser) in
  begin match parser.token with
    | (Token.AddOp "->") ->
      begin let pos = parser.pos in
      begin
      (lookahead parser);
      begin let rhs = (parse_fun_type parser) in
      ((TypeExpr.at pos) (TypeExpr.Fun (lhs, rhs)))
      end
      end
      end
    | _ ->
      lhs
  end
  end
end

and parse_tuple_type = begin fun parser ->
  begin let t = (parse_atomic_type parser) in
  begin let rec loop = begin fun ts ->
    begin match parser.token with
      | (Token.MulOp "*") ->
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
    | (Token.MulOp "*") ->
      begin let pos = parser.pos in
      ((TypeExpr.at pos) (TypeExpr.Tuple (loop (( :: ) (t, ( [] ))))))
      end
    | _ ->
      t
  end
  end
  end
end

and parse_atomic_type = begin fun parser ->
  begin match parser.token with
    | ((Token.LowId _) | (Token.CapId _)) ->
      begin let pos = parser.pos in
      begin let typector = ((parse_typector parser) ( [] )) in
      begin if ((( = ) parser.token) (Token.Reserved "(")) then
        begin
        (lookahead parser);
        begin let args = (((parse_elems parser) comma_or_rparen) parse_type) in
        ((TypeExpr.at pos) (TypeExpr.App (typector, args)))
        end
        end
      else
        ((TypeExpr.at pos) (TypeExpr.Con typector))
      end
      end
      end
    | (Token.Reserved "(") ->
      begin
      (lookahead parser);
      begin let t = (parse_type parser) in
      begin
      ((parse_token parser) (Token.Reserved ")"));
      t
      end
      end
      end
    | (Token.Reserved "`") ->
      (parse_type_var parser)
    | _ ->
      (failwith ((expected parser) "type"))
  end
end

and parse_typector = begin fun parser ->
  begin fun mod_names ->
    begin match parser.token with
      | (Token.LowId _) ->
        begin let typector_name = (parse_lowid parser) in
        ((List.rev mod_names), typector_name)
        end
      | (Token.CapId _) ->
        begin let capid = (parse_capid parser) in
        begin
        ((parse_token parser) (Token.Reserved "."));
        ((parse_typector parser) (( :: ) (capid, mod_names)))
        end
        end
      | _ ->
        (failwith ((expected parser) "identifier"))
    end
  end
end

and parse_type_var = begin fun parser ->
  begin let pos = parser.pos in
  begin
  ((parse_token parser) (Token.Reserved "`"));
  ((TypeExpr.at pos) (TypeExpr.Var (parse_lowid parser)))
  end
  end
end

and parse_type_params = begin fun parser ->
  begin if ((( = ) parser.token) (Token.Reserved "(")) then
    begin
    (lookahead parser);
    (((parse_elems parser) comma_or_rparen) parse_type_var)
    end
  else
    ( [] )
  end
end

let rec parse_pattern = begin fun parser ->
  (parse_as_pattern parser)
end

and parse_as_pattern = begin fun parser ->
  begin let pat = (parse_or_pattern parser) in
  begin if ((( = ) parser.token) (Token.Reserved "as")) then
    begin let pos = parser.pos in
    begin
    (lookahead parser);
    begin let name = (parse_val_name parser) in
    ((Pattern.at pos) (Pattern.As (pat, name)))
    end
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
    begin if ((( = ) parser.token) (Token.CmpOp "|")) then
      begin let pos = parser.pos in
      begin
      (lookahead parser);
      begin let rhs = (parse_cons_pattern parser) in
      (loop ((Pattern.at pos) (Pattern.Or (lhs, rhs))))
      end
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
  begin if ((( = ) parser.token) (Token.ConsOp "::")) then
    begin let pos = parser.pos in
    begin
    (lookahead parser);
    begin let rhs = (parse_cons_pattern parser) in
    (((make_cons_pattern pos) lhs) rhs)
    end
    end
    end
  else
    lhs
  end
  end
end

and parse_atomic_pattern = begin fun parser ->
  begin match parser.token with
    | (((((Token.Int _) | (Token.String _)) | (Token.Char _)) | (Token.Reserved "true")) | (Token.Reserved "false")) ->
      begin let pos = parser.pos in
      begin let lit = (parse_literal parser) in
      ((Pattern.at pos) (Pattern.Con lit))
      end
      end
    | (Token.LowId "_") ->
      begin let pos = parser.pos in
      begin
      (lookahead parser);
      ((Pattern.at pos) Pattern.WildCard)
      end
      end
    | (Token.LowId _) ->
      begin let pos = parser.pos in
      begin let name = (parse_val_name parser) in
      ((Pattern.at pos) (Pattern.Var name))
      end
      end
    | (Token.CapId _) ->
      (parse_variant_pattern parser)
    | (Token.Reserved "[") ->
      begin let pos = parser.pos in
      begin
      (lookahead parser);
      ((parse_list_pattern parser) pos)
      end
      end
    | (Token.Reserved "{") ->
      begin let pos = parser.pos in
      begin
      (lookahead parser);
      ((parse_record_pattern parser) pos)
      end
      end
    | (Token.Reserved "(") ->
      (parse_parens_pattern parser)
    | _ ->
      (failwith ((expected parser) "pattern"))
  end
end

and parse_variant_pattern = begin fun parser ->
  begin let pos = parser.pos in
  begin let ctor = ((parse_ctor parser) ( [] )) in
  begin if ((( = ) parser.token) (Token.Reserved "(")) then
    begin let pos_tuple = parser.pos in
    begin let pat = begin match (parse_params parser) with
      | ( [] ) ->
        (assert false)
      | (( :: ) (pat, ( [] ))) ->
        pat
      | pat_list ->
        ((Pattern.at pos_tuple) (Pattern.Tuple pat_list))
    end in
    ((Pattern.at pos) (Pattern.Ctor (ctor, (Some pat))))
    end
    end
  else
    ((Pattern.at pos) (Pattern.Ctor (ctor, None)))
  end
  end
  end
end

and parse_list_pattern = begin fun parser ->
  begin fun pos ->
    begin let list = (((parse_elems parser) semi_or_rbracket) parse_pattern) in
    (((List.fold_right (make_cons_pattern pos)) list) ((Pattern.at pos) nil_pattern))
    end
  end
end

and parse_record_pattern = begin fun parser ->
  begin fun pos ->
    ((Pattern.at pos) (Pattern.Record ((parse_braced_elems parser) parse_field_pattern)))
  end
end

and parse_field_pattern = begin fun parser ->
  begin let field_name = ((parse_val_path parser) ( [] )) in
  begin if ((( = ) parser.token) (Token.Reserved "=")) then
    begin
    (lookahead parser);
    begin let pattern = (parse_pattern parser) in
    (field_name, (Some pattern))
    end
    end
  else
    (field_name, None)
  end
  end
end

and parse_parens_pattern = begin fun parser ->
  begin let pos = parser.pos in
  begin match (parse_params parser) with
    | ( [] ) ->
      (assert false)
    | (( :: ) (pat, ( [] ))) ->
      pat
    | pat_list ->
      ((Pattern.at pos) (Pattern.Tuple pat_list))
  end
  end
end

and parse_params = begin fun parser ->
  begin let pos = parser.pos in
  begin
  ((parse_token parser) (Token.Reserved "("));
  begin if ((( = ) parser.token) (Token.Reserved ")")) then
    begin
    (lookahead parser);
    (( :: ) (((Pattern.at pos) unit_pattern), ( [] )))
    end
  else
    (parse_pattern_list parser)
  end
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
    | (Token.AssignOp ":=") ->
      begin let pos = parser.pos in
      begin
      (lookahead parser);
      begin let op = ((Expr.at pos) (make_op_var ":=")) in
      begin let rhs = (parse_assign_expr parser) in
      ((Expr.at pos) (Expr.App (((Expr.at pos) (Expr.App (op, lhs))), rhs)))
      end
      end
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
    | (Token.OrOp "||") ->
      begin let pos = parser.pos in
      begin
      (lookahead parser);
      begin let rhs = (parse_or_expr parser) in
      ((Expr.at pos) (Expr.Or (lhs, rhs)))
      end
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
    | (Token.AndOp "&&") ->
      begin let pos = parser.pos in
      begin
      (lookahead parser);
      begin let rhs = (parse_and_expr parser) in
      ((Expr.at pos) (Expr.And (lhs, rhs)))
      end
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
      | (Token.CmpOp str) ->
        (Some str)
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
    | (Token.ConsOp str) ->
      begin let pos = parser.pos in
      begin
      (lookahead parser);
      begin let rhs = (parse_cons_expr parser) in
      ((Expr.at pos) (Expr.Ctor (cons_op, (Some ((Expr.at pos) (Expr.Tuple (( :: ) (lhs, (( :: ) (rhs, ( [] )))))))))))
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
      | (Token.AddOp str) ->
        (Some str)
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
      | (Token.MulOp str) ->
        (Some str)
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
      | (Token.PowOp str) ->
        (Some str)
      | _ ->
        None
    end
  end in
  (((parse_right_assoc parser) get_op) parse_unary_expr)
  end
end

and parse_unary_expr = begin fun parser ->
  begin match parser.token with
    | (Token.AddOp "-") ->
      begin let pos = parser.pos in
      begin
      (lookahead parser);
      begin let expr = (parse_unary_expr parser) in
      ((Expr.at pos) (Expr.App (((Expr.at pos) (make_op_var "~-")), expr)))
      end
      end
      end
    | (Token.AddOp "+") ->
      begin let pos = parser.pos in
      begin
      (lookahead parser);
      begin let expr = (parse_unary_expr parser) in
      ((Expr.at pos) (Expr.App (((Expr.at pos) (make_op_var "~+")), expr)))
      end
      end
      end
    | (Token.CmpOp "!") ->
      begin let pos = parser.pos in
      begin
      (lookahead parser);
      begin let expr = (parse_unary_expr parser) in
      ((Expr.at pos) (Expr.App (((Expr.at pos) (make_op_var "!")), expr)))
      end
      end
      end
    | _ ->
      (parse_dot_expr parser)
  end
end

and parse_dot_expr = begin fun parser ->
  begin let expr = (parse_prim_expr parser) in
  begin let rec loop = begin fun expr ->
    begin match parser.token with
      | (Token.Reserved ".") ->
        begin let pos = parser.pos in
        begin
        (lookahead parser);
        begin match parser.token with
          | (Token.Reserved "{") ->
            begin
            (lookahead parser);
            begin let raw = (Expr.Update (expr, ((parse_braced_elems parser) parse_field_def))) in
            (loop ((Expr.at pos) raw))
            end
            end
          | _ ->
            begin let path = ((parse_val_path parser) ( [] )) in
            begin match parser.token with
              | (Token.AssignOp "<-") ->
                begin
                (lookahead parser);
                begin let rhs = (parse_expr parser) in
                ((Expr.at pos) (Expr.Assign (expr, path, rhs)))
                end
                end
              | _ ->
                (loop ((Expr.at pos) (Expr.Field (expr, path))))
            end
            end
        end
        end
        end
      | _ ->
        expr
    end
  end in
  (loop expr)
  end
  end
end

and parse_prim_expr = begin fun parser ->
  begin let fun_expr = (parse_atomic_expr parser) in
  begin let rec loop = begin fun fun_expr ->
    begin match parser.token with
      | (Token.Reserved "(") ->
        begin let pos = parser.pos in
        begin
        (lookahead parser);
        begin let arg_exprs = ((parse_args parser) pos) in
        (loop (((make_app pos) fun_expr) arg_exprs))
        end
        end
        end
      | (Token.Reserved "^") ->
        begin let pos = parser.pos in
        begin
        (lookahead parser);
        begin let arg_expr = ((parse_abs parser) pos) in
        (loop ((Expr.at pos) (Expr.App (fun_expr, arg_expr))))
        end
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

and parse_atomic_expr = begin fun parser ->
  begin match parser.token with
    | (((((Token.Int _) | (Token.String _)) | (Token.Char _)) | (Token.Reserved "true")) | (Token.Reserved "false")) ->
      begin let pos = parser.pos in
      begin let lit = (parse_literal parser) in
      ((Expr.at pos) (Expr.Con lit))
      end
      end
    | (((Token.LowId _) | (Token.CapId _)) | (Token.Reserved "$")) ->
      ((parse_var_or_ctor_app parser) ( [] ))
    | (Token.Reserved "^") ->
      begin let pos = parser.pos in
      begin
      (lookahead parser);
      ((parse_abs parser) pos)
      end
      end
    | (Token.Reserved "[") ->
      begin let pos = parser.pos in
      begin
      (lookahead parser);
      ((parse_list parser) pos)
      end
      end
    | (Token.Reserved "(") ->
      begin let pos = parser.pos in
      begin
      (lookahead parser);
      ((parse_parens parser) pos)
      end
      end
    | (Token.Reserved "{") ->
      begin let pos = parser.pos in
      begin
      (lookahead parser);
      ((parse_record parser) pos)
      end
      end
    | (Token.Reserved "if") ->
      begin let pos = parser.pos in
      begin
      (lookahead parser);
      ((parse_if_expr parser) pos)
      end
      end
    | (Token.Reserved "match") ->
      begin let pos = parser.pos in
      begin
      (lookahead parser);
      ((parse_match_expr parser) pos)
      end
      end
    | (Token.Reserved "try") ->
      begin let pos = parser.pos in
      begin
      (lookahead parser);
      ((parse_try_expr parser) pos)
      end
      end
    | _ ->
      (failwith ((expected parser) "expression"))
  end
end

and parse_var_or_ctor_app = begin fun parser ->
  begin fun mod_names ->
    begin match parser.token with
      | ((Token.LowId _) | (Token.Reserved "$")) ->
        begin let pos = parser.pos in
        begin let val_name = (parse_val_name parser) in
        ((Expr.at pos) (Expr.Var ((List.rev mod_names), val_name)))
        end
        end
      | (Token.CapId _) ->
        begin let pos = parser.pos in
        begin let capid = (parse_capid parser) in
        begin if ((( = ) parser.token) (Token.Reserved ".")) then
          begin
          (lookahead parser);
          ((parse_var_or_ctor_app parser) (( :: ) (capid, mod_names)))
          end
        else
          begin let ctor = ((List.rev mod_names), (Names.Id capid)) in
          (((parse_ctor_app parser) pos) ctor)
          end
        end
        end
        end
      | _ ->
        (failwith ((expected parser) "identifier"))
    end
  end
end

and parse_ctor_app = begin fun parser ->
  begin fun pos ->
    begin fun ctor ->
      begin match parser.token with
        | (Token.Reserved "(") ->
          begin let pos_paren = parser.pos in
          begin
          (lookahead parser);
          begin let arg_expr = begin match ((parse_args parser) pos_paren) with
            | ( [] ) ->
              (assert false)
            | (( :: ) (arg_expr, ( [] ))) ->
              arg_expr
            | arg_exprs ->
              ((Expr.at pos_paren) (Expr.Tuple arg_exprs))
          end in
          ((Expr.at pos) (Expr.Ctor (ctor, (Some arg_expr))))
          end
          end
          end
        | (Token.Reserved "^") ->
          begin
          (lookahead parser);
          begin let arg_expr = ((parse_abs parser) pos) in
          ((Expr.at pos) (Expr.Ctor (ctor, (Some arg_expr))))
          end
          end
        | _ ->
          ((Expr.at pos) (Expr.Ctor (ctor, None)))
      end
    end
  end
end

and parse_abs = begin fun parser ->
  begin fun pos ->
    begin let params = (parse_params parser) in
    begin let body_expr = (parse_block parser) in
    (((make_abs pos) params) body_expr)
    end
    end
  end
end

and parse_block = begin fun parser ->
  begin match parser.token with
    | (Token.Reserved ":") ->
      begin
      (Lexer.indent parser.lexer);
      begin
      (lookahead parser);
      (parse_indented_block parser)
      end
      end
    | (Token.Reserved "{") ->
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
  ((parse_token parser) (Token.Reserved "}"));
  expr
  end
  end
end

and parse_block_elem = begin fun parser ->
  begin fun sep_or_term ->
    begin match parser.token with
      | (Token.Reserved "var") ->
        begin let pos = parser.pos in
        begin
        (lookahead parser);
        begin let (val_name, val_expr) = (parse_let_val parser) in
        begin
        ((parse_sep parser) sep_or_term);
        begin let cont_expr = ((parse_block_elem parser) sep_or_term) in
        ((Expr.at pos) (Expr.LetVal (val_name, val_expr, cont_expr)))
        end
        end
        end
        end
        end
      | (Token.Reserved "def") ->
        begin let pos = parser.pos in
        begin let defs = (( :: ) ((parse_let_fun parser), ( [] ))) in
        begin
        ((parse_sep parser) sep_or_term);
        begin let cont_expr = ((parse_block_elem parser) sep_or_term) in
        ((Expr.at pos) (Expr.LetFun (defs, cont_expr)))
        end
        end
        end
        end
      | (Token.Reserved "rec") ->
        begin let pos = parser.pos in
        begin
        (lookahead parser);
        begin let defs = ((parse_block_like_elems parser) parse_let_fun) in
        begin
        ((parse_sep parser) sep_or_term);
        begin let cont_expr = ((parse_block_elem parser) sep_or_term) in
        ((Expr.at pos) (Expr.LetFun (defs, cont_expr)))
        end
        end
        end
        end
        end
      | _ ->
        begin let lhs = (parse_expr parser) in
        begin match (sep_or_term parser.token) with
          | Sep ->
            begin let pos = parser.pos in
            begin
            (lookahead parser);
            begin match (sep_or_term parser.token) with
              | Term ->
                lhs
              | _ ->
                begin let rhs = ((parse_block_elem parser) sep_or_term) in
                ((Expr.at pos) (Expr.Seq (lhs, rhs)))
                end
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
  ((parse_token parser) (Token.CmpOp "="));
  begin let val_expr = (parse_expr parser) in
  (val_pat, val_expr)
  end
  end
  end
end

and parse_let_fun = begin fun parser ->
  begin
  ((parse_token parser) (Token.Reserved "def"));
  begin let pos = parser.pos in
  begin let fun_name = (parse_val_name parser) in
  begin let params = (parse_params parser) in
  begin let body_expr = (parse_block parser) in
  (fun_name, (((make_abs pos) params) body_expr))
  end
  end
  end
  end
  end
end

and parse_sep = begin fun parser ->
  begin fun sep_or_term ->
    begin match (sep_or_term parser.token) with
      | Sep ->
        (lookahead parser)
      | _ ->
        (failwith ((expected parser) "separator"))
    end
  end
end

and parse_list = begin fun parser ->
  begin fun pos ->
    begin let list = (((parse_elems parser) semi_or_rbracket) parse_expr) in
    begin let rec make_cons = begin fun elem ->
      begin fun acc ->
        ((Expr.at pos) (Expr.Ctor (cons_op, (Some ((Expr.at pos) (Expr.Tuple (( :: ) (elem, (( :: ) (acc, ( [] )))))))))))
      end
    end in
    (((List.fold_right make_cons) list) ((Expr.at pos) nil_expr))
    end
    end
  end
end

and parse_parens = begin fun parser ->
  begin fun pos ->
    begin match ((parse_args parser) pos) with
      | ( [] ) ->
        (assert false)
      | (( :: ) (arg_expr, ( [] ))) ->
        arg_expr
      | arg_exprs ->
        ((Expr.at pos) (Expr.Tuple arg_exprs))
    end
  end
end

and parse_args = begin fun parser ->
  begin fun pos ->
    begin if ((( = ) parser.token) (Token.Reserved ")")) then
      begin
      (lookahead parser);
      (( :: ) (((Expr.at pos) unit_expr), ( [] )))
      end
    else
      (parse_expr_list parser)
    end
  end
end

and parse_expr_list = begin fun parser ->
  (((parse_elems parser) comma_or_rparen) parse_expr)
end

and parse_record = begin fun parser ->
  begin fun pos ->
    ((Expr.at pos) (Expr.Record ((parse_braced_elems parser) parse_field_def)))
  end
end

and parse_field_def = begin fun parser ->
  begin let field_name = ((parse_val_path parser) ( [] )) in
  begin
  ((parse_token parser) (Token.CmpOp "="));
  begin let expr = (parse_expr parser) in
  (field_name, expr)
  end
  end
  end
end

and parse_if_expr = begin fun parser ->
  begin fun pos ->
    begin let cond_expr = (parse_expr parser) in
    begin let then_expr = (parse_block parser) in
    begin
    ((skip parser) Token.Newline);
    begin
    ((parse_token parser) (Token.Reserved "else"));
    begin let else_expr = (parse_block parser) in
    ((Expr.at pos) (Expr.If (cond_expr, then_expr, else_expr)))
    end
    end
    end
    end
    end
  end
end

and parse_match_expr = begin fun parser ->
  begin fun pos ->
    begin let target_expr = (parse_expr parser) in
    begin let cases = ((parse_block_like_elems parser) parse_case) in
    ((Expr.at pos) (Expr.Match (target_expr, cases)))
    end
    end
  end
end

and parse_try_expr = begin fun parser ->
  begin fun pos ->
    begin let expr = (parse_block parser) in
    begin
    ((skip parser) Token.Newline);
    begin
    ((parse_token parser) (Token.Reserved "with"));
    begin let cases = ((parse_block_like_elems parser) parse_case) in
    ((Expr.at pos) (Expr.Try (expr, cases)))
    end
    end
    end
    end
  end
end

and parse_case = begin fun parser ->
  begin
  ((parse_token parser) (Token.Reserved "case"));
  begin let pat = (parse_pattern parser) in
  begin if ((( = ) parser.token) (Token.Reserved "when")) then
    begin
    (lookahead parser);
    begin let guard = (parse_expr parser) in
    begin let expr = (parse_block parser) in
    (pat, (Some guard), expr)
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
    | (Token.Reserved "var") ->
      begin let pos = parser.pos in
      begin
      (lookahead parser);
      ((parse_top_let_val parser) pos)
      end
      end
    | (Token.Reserved "def") ->
      begin let pos = parser.pos in
      ((Top.at pos) (Top.LetFun (( :: ) ((parse_top_let_fun parser), ( [] )))))
      end
    | (Token.Reserved "type") ->
      begin let pos = parser.pos in
      ((Top.at pos) (Top.Type (( :: ) ((parse_top_type_def parser), ( [] )))))
      end
    | (Token.Reserved "open") ->
      begin let pos = parser.pos in
      begin
      (lookahead parser);
      ((parse_top_open parser) pos)
      end
      end
    | (Token.Reserved "exception") ->
      begin let pos = parser.pos in
      begin
      (lookahead parser);
      ((Top.at pos) (Top.Exception ((parse_exn_decl parser) pos)))
      end
      end
    | (Token.Reserved "rec") ->
      begin let pos = parser.pos in
      begin
      (lookahead parser);
      ((parse_top_rec parser) pos)
      end
      end
    | _ ->
      begin let pos = parser.pos in
      ((Top.at pos) (Top.Expr (parse_expr parser)))
      end
  end
end

and parse_top_let_val = begin fun parser ->
  begin fun pos ->
    begin let val_pat = (parse_pattern parser) in
    begin
    ((parse_token parser) (Token.CmpOp "="));
    ((Top.at pos) (Top.LetVal (val_pat, (parse_expr parser))))
    end
    end
  end
end

and parse_top_let_fun = begin fun parser ->
  begin
  ((parse_token parser) (Token.Reserved "def"));
  begin let pos = parser.pos in
  begin let fun_name = (parse_val_name parser) in
  begin let params = (parse_params parser) in
  begin let body_expr = (parse_block parser) in
  (fun_name, (((make_abs pos) params) body_expr))
  end
  end
  end
  end
  end
end

and parse_top_open = begin fun parser ->
  begin fun pos ->
    begin let mod_path = ((parse_mod_path parser) ( [] )) in
    ((Top.at pos) (Top.Open mod_path))
    end
  end
end

and parse_exn_decl = begin fun parser ->
  begin fun pos ->
    begin let exn_name = (Names.Id (parse_capid parser)) in
    begin let ret_type_expr = ((TypeExpr.at pos) exn_type_expr) in
    begin if ((( = ) parser.token) (Token.Reserved "(")) then
      begin
      (lookahead parser);
      begin let t = (parse_type parser) in
      begin
      ((parse_token parser) (Token.Reserved ")"));
      begin let ctor_type_expr = ((TypeExpr.at pos) (TypeExpr.Fun (t, ret_type_expr))) in
      (exn_name, (Some t), ctor_type_expr)
      end
      end
      end
      end
    else
      (exn_name, None, ret_type_expr)
    end
    end
    end
  end
end

and parse_top_rec = begin fun parser ->
  begin fun pos ->
    begin match parser.token with
      | (Token.Reserved ":") ->
        begin
        (Lexer.indent parser.lexer);
        begin
        (lookahead parser);
        begin match parser.token with
          | (Token.Reserved "type") ->
            ((Top.at pos) (Top.Type ((parse_indented_elems parser) parse_top_type_def)))
          | _ ->
            ((Top.at pos) (Top.LetFun ((parse_indented_elems parser) parse_top_let_fun)))
        end
        end
        end
      | (Token.Reserved "{") ->
        begin
        (lookahead parser);
        begin match parser.token with
          | (Token.Reserved "type") ->
            ((Top.at pos) (Top.Type ((parse_braced_elems parser) parse_top_type_def)))
          | _ ->
            ((Top.at pos) (Top.LetFun ((parse_braced_elems parser) parse_top_let_fun)))
        end
        end
      | _ ->
        (failwith ((expected parser) "':' or '{'"))
    end
  end
end

and parse_top_type_def = begin fun parser ->
  begin match (parse_type_def parser) with
    | (Left (typector_name, type_params)) ->
      (failwith ((expected parser) "'=' or ':' or '{'"))
    | (Right type_def) ->
      type_def
  end
end

and parse_type_def = begin fun parser ->
  begin
  ((parse_token parser) (Token.Reserved "type"));
  begin let pos = parser.pos in
  begin let typector_name = (parse_lowid parser) in
  begin let type_params = (parse_type_params parser) in
  begin let defined_type = begin if ((( = ) (List.length type_params)) 0) then
    ((TypeExpr.at pos) (TypeExpr.Con (( [] ), typector_name)))
  else
    ((TypeExpr.at pos) (TypeExpr.App ((( [] ), typector_name), type_params)))
  end in
  begin match parser.token with
    | (Token.CmpOp "=") ->
      begin
      (lookahead parser);
      begin let t = (parse_type parser) in
      begin let conv_fun_type_expr = ((TypeExpr.at pos) (TypeExpr.Fun (defined_type, t))) in
      (Right (TypeDef.Abbrev (typector_name, type_params, t, conv_fun_type_expr)))
      end
      end
      end
    | ((Token.Reserved ":") | (Token.Reserved "{")) ->
      begin let type_info = ((parse_type_info parser) defined_type) in
      (Right (TypeDef.Repr (typector_name, type_params, type_info)))
      end
    | _ ->
      (Left (typector_name, type_params))
  end
  end
  end
  end
  end
  end
end

and parse_type_info = begin fun parser ->
  begin fun defined_type ->
    begin match parser.token with
      | (Token.Reserved ":") ->
        begin
        (Lexer.indent parser.lexer);
        begin
        (lookahead parser);
        begin match parser.token with
          | (Token.Reserved "def") ->
            (TypeInfo.Variant ((parse_indented_elems parser) (parse_ctor_decl defined_type)))
          | _ ->
            (TypeInfo.Record ((parse_indented_elems parser) (parse_field_decl defined_type)))
        end
        end
        end
      | (Token.Reserved "{") ->
        begin
        (lookahead parser);
        begin match parser.token with
          | (Token.Reserved "def") ->
            (TypeInfo.Variant ((parse_braced_elems parser) (parse_ctor_decl defined_type)))
          | _ ->
            (TypeInfo.Record ((parse_braced_elems parser) (parse_field_decl defined_type)))
        end
        end
      | _ ->
        (failwith ((expected parser) "':' or '{'"))
    end
  end
end

and parse_ctor_decl = begin fun ret_type ->
  begin fun parser ->
    begin let pos = parser.pos in
    begin
    ((parse_token parser) (Token.Reserved "def"));
    begin let ctor_name = (parse_ctor_name parser) in
    begin if ((( = ) parser.token) (Token.Reserved "(")) then
      begin
      (lookahead parser);
      begin let param_type = (parse_type parser) in
      begin
      ((parse_token parser) (Token.Reserved ")"));
      (ctor_name, (Some param_type), ((TypeExpr.at pos) (TypeExpr.Fun (param_type, ret_type))))
      end
      end
      end
    else
      (ctor_name, None, ret_type)
    end
    end
    end
    end
  end
end

and parse_field_decl = begin fun record_type ->
  begin fun parser ->
    begin let pos = parser.pos in
    begin let is_mutable = ((( = ) parser.token) (Token.Reserved "mutable")) in
    begin
    begin if is_mutable then
      (lookahead parser)
    else
      ()
    end;
    begin let field_name = (parse_val_name parser) in
    begin
    ((parse_token parser) (Token.Reserved ":"));
    begin let t = (parse_type parser) in
    (is_mutable, field_name, t, ((TypeExpr.at pos) (TypeExpr.Fun (record_type, t))))
    end
    end
    end
    end
    end
    end
  end
end

let rec parse_decl_expr = begin fun parser ->
  begin match parser.token with
    | (Token.Reserved "val") ->
      begin
      (lookahead parser);
      begin let name = (parse_val_name parser) in
      begin
      ((parse_token parser) (Token.Reserved ":"));
      begin let type_expr = (parse_type parser) in
      (DeclExpr.Val (name, type_expr))
      end
      end
      end
      end
    | (Token.Reserved "type") ->
      begin match (parse_type_def parser) with
        | (Left (typector_name, type_params)) ->
          (DeclExpr.AbstrType (typector_name, (List.length type_params)))
        | (Right type_def) ->
          (DeclExpr.ConcrType (( :: ) (type_def, ( [] ))))
      end
    | (Token.Reserved "exception") ->
      begin let pos = parser.pos in
      begin
      (lookahead parser);
      (DeclExpr.Exception ((parse_exn_decl parser) pos))
      end
      end
    | _ ->
      (failwith ((expected parser) "declaration"))
  end
end

let rec parse_decl_stmt = begin fun parser ->
  begin let decl_expr = (parse_decl_expr parser) in
  begin match parser.token with
    | ((Token.EOF | Token.Newline) | (Token.Reserved ";")) ->
      decl_expr
    | _ ->
      (failwith ((expected parser) "newline or ';'"))
  end
  end
end

let rec parse_decl = begin fun parser ->
  begin
  (lookahead parser);
  begin match parser.token with
    | Token.EOF ->
      None
    | _ ->
      (Some (parse_decl_stmt parser))
  end
  end
end

let rec parse_stmt = begin fun parser ->
  begin let expr = (parse_top parser) in
  begin match parser.token with
    | ((Token.EOF | Token.Newline) | (Token.Reserved ";")) ->
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
    | Token.EOF ->
      None
    | _ ->
      (Some (parse_stmt parser))
  end
  end
end

