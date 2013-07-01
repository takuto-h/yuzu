open Printf

type require_argument = bool

type mutability = bool

type t = {
  mods : ((Names.mod_name * Module.t)) list;
  curr_mod : Module.t;
  mod_name : Names.mod_name;
  opens : ((Names.mod_name * Names.mod_path)) list;
  let_level : int;
}

let default_opens = (( :: ) (("Pervasives", ( [] )), ( [] )))

let rec create = begin fun mods ->
  {
    mods = mods;
    curr_mod = ((((((Module.make ( [] )) ( [] )) ( [] )) ( [] )) ( [] )) ( [] ));
    mod_name = "Dummy";
    opens = default_opens;
    let_level = 0;
  }
end

let rec incr_let_level = begin fun inf ->
  {
    inf with
    let_level = ((( + ) inf.let_level) 1);
  }
end

let unit_type = (Type.Con ((( :: ) ("Pervasives", ( [] ))), "unit"))

let int_type = (Type.Con ((( :: ) ("Pervasives", ( [] ))), "int"))

let string_type = (Type.Con ((( :: ) ("Pervasives", ( [] ))), "string"))

let char_type = (Type.Con ((( :: ) ("Pervasives", ( [] ))), "char"))

let bool_type = (Type.Con ((( :: ) ("Pervasives", ( [] ))), "bool"))

let exn_type = (Type.Con ((( :: ) ("Pervasives", ( [] ))), "exn"))

let rec unbound_variable = begin fun pos ->
  begin fun path ->
    ((Pos.show_error pos) ((sprintf "unbound variable: %s\n") (Names.show_val_path path)))
  end
end

let rec unbound_constructor = begin fun pos ->
  begin fun ctor ->
    ((Pos.show_error pos) ((sprintf "unbound constructor: %s\n") (Names.show_ctor ctor)))
  end
end

let rec unbound_field_label = begin fun pos ->
  begin fun path ->
    ((Pos.show_error pos) ((sprintf "unbound field label: %s\n") (Names.show_val_path path)))
  end
end

let rec unbound_type_constructor = begin fun pos ->
  begin fun typector ->
    ((Pos.show_error pos) ((sprintf "unbound type constructor: %s\n") (Names.show_typector typector)))
  end
end

let rec unbound_type_class = begin fun pos ->
  begin fun typeclass ->
    ((Pos.show_error pos) ((sprintf "unbound type class: %s\n") (Names.show_typeclass typeclass)))
  end
end

let rec invalid_application = begin fun pos ->
  begin fun fun_type ->
    begin fun arg_type ->
      begin fun t1 ->
        begin fun t2 ->
          begin let shower = (Type.create_shower 0) in
          ((((sprintf "%s%s%s") ((Pos.show_error pos) (((sprintf "invalid application\n%s%s") ((sprintf "function type: %s\n") ((Type.show shower) fun_type))) ((sprintf "argument type: %s\n") ((Type.show shower) arg_type))))) (((Type.show_origin shower) "function type") t1)) (((Type.show_origin shower) "argument type") t2))
          end
        end
      end
    end
  end
end

let rec wrong_number_of_arguments = begin fun pos ->
  begin fun got ->
    begin fun req ->
      ((Pos.show_error pos) (((sprintf "wrong number of arguments (%d for %d)\n") got) req))
    end
  end
end

let rec invalid_if_expr = begin fun pos ->
  begin fun then_type ->
    begin fun else_type ->
      begin fun t1 ->
        begin fun t2 ->
          begin let shower = (Type.create_shower 0) in
          ((((sprintf "%s%s%s") ((Pos.show_error pos) (((sprintf "invalid if expression\n%s%s") ((sprintf "then-clause type: %s\n") ((Type.show shower) then_type))) ((sprintf "else-clause type: %s\n") ((Type.show shower) else_type))))) (((Type.show_origin shower) "then-clause type") t1)) (((Type.show_origin shower) "else-clause type") t2))
          end
        end
      end
    end
  end
end

let rec required = begin fun pos ->
  begin fun req_type ->
    begin fun got_type ->
      begin fun t1 ->
        begin fun t2 ->
          begin let shower = (Type.create_shower 0) in
          ((((sprintf "%s%s%s") ((Pos.show_error pos) (((sprintf "'%s' required, but got '%s'\n") ((Type.show shower) req_type)) ((Type.show shower) got_type)))) (((Type.show_origin shower) "required type") t1)) (((Type.show_origin shower) "got type") t2))
          end
        end
      end
    end
  end
end

let rec inconsistent_types = begin fun pos ->
  begin fun type1 ->
    begin fun type2 ->
      begin fun t1 ->
        begin fun t2 ->
          begin let shower = (Type.create_shower 0) in
          ((((sprintf "%s%s%s") ((Pos.show_error pos) (((sprintf "inconsistent types\n%s%s") ((sprintf "type of lhs: %s\n") ((Type.show shower) type1))) ((sprintf "type of rhs: %s\n") ((Type.show shower) type2))))) (((Type.show_origin shower) "type of lhs") t1)) (((Type.show_origin shower) "type of rhs") t2))
          end
        end
      end
    end
  end
end

let rec inconsistent_var_occurrence = begin fun pos ->
  ((Pos.show_error pos) "inconsistent variable occurrence\n")
end

let rec field_not_mutable = begin fun pos ->
  begin fun path ->
    ((Pos.show_error pos) ((sprintf "field not mutable: %s\n") (Names.show_val_path path)))
  end
end

let rec instance_not_found = begin fun pos ->
  begin fun tc ->
    begin fun t ->
      begin let shower = (Type.create_shower 0) in
      (((sprintf "%s%s") ((Pos.show_error pos) (((sprintf "instance not found: %s(%s)\n") (Names.show_typeclass tc)) ((Type.show shower) t)))) (((Type.show_origin shower) "constraint") t))
      end
    end
  end
end

let rec search_mods = begin fun search_mod ->
  begin fun mods ->
    begin fun mod_name ->
      begin fun mod_path ->
        begin fun name ->
          begin let modl = ((List.assoc mod_name) mods) in
          (((search_mod modl) mod_path) name)
          end
        end
      end
    end
  end
end

let rec search_opens = begin fun search_mod ->
  begin fun mods ->
    begin fun opens ->
      begin fun name ->
        begin match opens with
          | ( [] ) ->
            (raise Not_found)
          | (( :: ) ((mod_name, mod_path), opens)) ->
            begin try
              (((((search_mods search_mod) mods) mod_name) mod_path) name)
            with
              | Not_found ->
                ((((search_opens search_mod) mods) opens) name)
            end
        end
      end
    end
  end
end

let rec search_alist = begin fun search_mod ->
  begin fun alist ->
    begin fun inf ->
      begin fun path ->
        begin match path with
          | (( [] ), name) ->
            begin try
              ((List.assoc name) alist)
            with
              | Not_found ->
                ((((search_opens search_mod) inf.mods) inf.opens) name)
            end
          | ((( :: ) (mod_name, mod_path)), name) ->
            (((((search_mods search_mod) inf.mods) mod_name) mod_path) name)
        end
      end
    end
  end
end

let rec search_asp = begin fun inf ->
  begin fun path ->
    ((((search_alist Module.search_asp) inf.curr_mod.Module.asp) inf) path)
  end
end

let rec search_ctors = begin fun inf ->
  begin fun ctor ->
    ((((search_alist Module.search_ctors) inf.curr_mod.Module.ctors) inf) ctor)
  end
end

let rec search_fields = begin fun inf ->
  begin fun path ->
    ((((search_alist Module.search_fields) inf.curr_mod.Module.fields) inf) path)
  end
end

let rec search_typectors = begin fun inf ->
  begin fun typector ->
    ((((search_alist Module.search_typectors) inf.curr_mod.Module.typectors) inf) typector)
  end
end

let rec search_typeclasses = begin fun inf ->
  begin fun typeclass ->
    ((((search_alist Module.search_typeclasses) inf.curr_mod.Module.typeclasses) inf) typeclass)
  end
end

let rec add_asp = begin fun inf ->
  begin fun name ->
    begin fun scm ->
      {
        inf with
        curr_mod = {
          inf.curr_mod with
          Module.asp = (( :: ) ((name, scm), inf.curr_mod.Module.asp));
        };
      }
    end
  end
end

let rec add_ctor = begin fun inf ->
  begin fun name ->
    begin fun info ->
      {
        inf with
        curr_mod = {
          inf.curr_mod with
          Module.ctors = (( :: ) ((name, info), inf.curr_mod.Module.ctors));
        };
      }
    end
  end
end

let rec add_field = begin fun inf ->
  begin fun name ->
    begin fun info ->
      {
        inf with
        curr_mod = {
          inf.curr_mod with
          Module.fields = (( :: ) ((name, info), inf.curr_mod.Module.fields));
        };
      }
    end
  end
end

let rec add_typector = begin fun inf ->
  begin fun name ->
    begin fun info ->
      {
        inf with
        curr_mod = {
          inf.curr_mod with
          Module.typectors = (( :: ) ((name, info), inf.curr_mod.Module.typectors));
        };
      }
    end
  end
end

let rec add_type_var = begin fun inf ->
  begin fun name ->
    begin let t = (Type.make_var inf.let_level) in
    begin let inf = (((add_asp inf) name) (Scheme.mono t)) in
    (inf, t)
    end
    end
  end
end

let rec apply = begin fun let_level ->
  begin fun pos ->
    begin fun fun_type ->
      begin fun arg_type ->
        begin let ret_type = (Type.make_var let_level) in
        begin
        begin try
          ((Type.unify fun_type) ((Type.at None) (Type.Fun (arg_type, ret_type))))
        with
          | (Type.Unification_error (t1, t2)) ->
            (failwith (((((invalid_application pos) fun_type) arg_type) t1) t2))
        end;
        ret_type
        end
        end
      end
    end
  end
end

let rec require_consistent = begin fun pos ->
  begin fun type1 ->
    begin fun type2 ->
      begin try
        ((Type.unify type1) type2)
      with
        | (Type.Unification_error (t1, t2)) ->
          (failwith (((((inconsistent_types pos) type1) type2) t1) t2))
      end
    end
  end
end

let rec instantiate = begin fun let_level ->
  begin fun {Scheme.gen_num;Scheme.preds;Scheme.body;} ->
    begin let type_vars = ((Array.init gen_num) begin fun _ ->
      (Type.make_var let_level)
    end) in
    begin let rec var_func = begin fun t ->
      begin fun _ ->
        begin fun _ ->
          t
        end
      end
    end in
    begin let rec gen_func = begin fun t ->
      begin fun n ->
        ((Array.get type_vars) n)
      end
    end in
    begin let preds = ((List.map begin fun (tc, t) ->
      (tc, (((Type.map var_func) gen_func) t))
    end) preds) in
    begin let body = (((Type.map var_func) gen_func) body) in
    (preds, body)
    end
    end
    end
    end
    end
  end
end

let rec generalize = begin fun let_level ->
  begin fun preds ->
    begin fun t ->
      begin let alist_ref = (ref ( [] )) in
      begin let rec var_func = begin fun t ->
        begin fun lv ->
          begin fun ref ->
            begin if ((( > ) lv) let_level) then
              begin try
                ((List.assq ref) (( ! ) alist_ref))
              with
                | Not_found ->
                  begin let gen = ((Type.at t.Type.pos) (Type.Gen (List.length (( ! ) alist_ref)))) in
                  begin
                  ((( := ) alist_ref) (( :: ) ((ref, gen), (( ! ) alist_ref))));
                  gen
                  end
                  end
              end
            else
              t
            end
          end
        end
      end in
      begin let rec gen_func = begin fun t ->
        begin fun _ ->
          (assert false)
        end
      end in
      begin let preds = ((List.map begin fun (tc, t) ->
        (tc, (((Type.map var_func) gen_func) t))
      end) preds) in
      (((Scheme.poly (List.length (( ! ) alist_ref))) preds) (((Type.map var_func) gen_func) t))
      end
      end
      end
      end
    end
  end
end

let rec infer_literal = begin fun lit ->
  begin match lit with
    | Literal.Unit ->
      unit_type
    | (Literal.Int _) ->
      int_type
    | (Literal.String _) ->
      string_type
    | (Literal.Char _) ->
      char_type
    | (Literal.Bool _) ->
      bool_type
  end
end

let rec infer_pattern = begin fun inf ->
  begin fun pat ->
    begin match pat.Pattern.raw with
      | Pattern.WildCard ->
        (inf, (Type.make_var inf.let_level), ValNameMap.empty)
      | (Pattern.Con lit) ->
        (inf, ((Type.at (Some pat.Pattern.pos)) (infer_literal lit)), ValNameMap.empty)
      | (Pattern.Var name) ->
        begin let (inf, t) = ((add_type_var inf) name) in
        (inf, t, ((ValNameMap.singleton name) t))
        end
      | (Pattern.Tuple pats) ->
        begin let init = (inf, ( [] ), ValNameMap.empty) in
        begin let (inf, ts, map) = (((YzList.fold_right pats) init) begin fun elem ->
          begin fun (inf, ts, map1) ->
            begin let (inf, t, map2) = ((infer_pattern inf) elem) in
            (inf, (( :: ) (t, ts)), (((ValNameMap.merge begin fun _ ->
              YzOption.or_
            end) map1) map2))
            end
          end
        end) in
        (inf, ((Type.at (Some pat.Pattern.pos)) (Type.Tuple ts)), map)
        end
        end
      | (Pattern.Ctor (ctor, opt_pat)) ->
        begin try
          begin let (req_arg, ctor_scm) = ((search_ctors inf) ctor) in
          begin let (preds0, ctor_type) = ((instantiate inf.let_level) ctor_scm) in
          begin
          (assert ((( = ) preds0) ( [] )));
          begin match (req_arg, opt_pat) with
            | (false, None) ->
              (inf, ctor_type, ValNameMap.empty)
            | (false, (Some pat)) ->
              (failwith (((wrong_number_of_arguments pat.Pattern.pos) 0) 1))
            | (true, None) ->
              (failwith (((wrong_number_of_arguments pat.Pattern.pos) 1) 0))
            | (true, (Some pat)) ->
              begin let (inf, param_type, map) = ((infer_pattern inf) pat) in
              begin let ret_type = ((((apply inf.let_level) pat.Pattern.pos) ctor_type) param_type) in
              (inf, ret_type, map)
              end
              end
          end
          end
          end
          end
        with
          | Not_found ->
            (failwith ((unbound_constructor pat.Pattern.pos) ctor))
        end
      | (Pattern.Record fields) ->
        begin let record_type = (Type.make_var inf.let_level) in
        begin let init = (inf, ValNameMap.empty) in
        begin let (inf, map) = (((YzList.fold_left init) fields) begin fun (inf, map1) ->
          begin fun (path, opt_pat) ->
            begin try
              begin let (mod_path, name) = path in
              begin let (is_mutable, access_fun_scm) = ((search_fields inf) path) in
              begin let (preds0, access_fun_type) = ((instantiate inf.let_level) access_fun_scm) in
              begin
              (assert ((( = ) preds0) ( [] )));
              begin
              (ignore ((((apply inf.let_level) pat.Pattern.pos) access_fun_type) record_type));
              begin match opt_pat with
                | None ->
                  begin let (inf, t) = ((add_type_var inf) name) in
                  (inf, (((ValNameMap.add name) t) map1))
                  end
                | (Some pat) ->
                  begin let (inf, t, map2) = ((infer_pattern inf) pat) in
                  (inf, (((ValNameMap.merge begin fun _ ->
                    YzOption.or_
                  end) map1) map2))
                  end
              end
              end
              end
              end
              end
              end
            with
              | Not_found ->
                (failwith ((unbound_field_label pat.Pattern.pos) path))
            end
          end
        end) in
        (inf, record_type, map)
        end
        end
        end
      | (Pattern.As (pat, name)) ->
        begin let (inf, t, map) = ((infer_pattern inf) pat) in
        begin let inf = (((add_asp inf) name) (Scheme.mono t)) in
        (inf, t, (((ValNameMap.add name) t) map))
        end
        end
      | (Pattern.Or (lhs, rhs)) ->
        begin let (lhs_inf, lhs_type, lhs_map) = ((infer_pattern inf) lhs) in
        begin let (rhs_inf, rhs_type, rhs_map) = ((infer_pattern inf) rhs) in
        begin if (not ((ValNameMap.equal_keys lhs_map) rhs_map)) then
          (failwith (inconsistent_var_occurrence pat.Pattern.pos))
        else
          begin
          (((require_consistent pat.Pattern.pos) lhs_type) rhs_type);
          (rhs_inf, rhs_type, rhs_map)
          end
        end
        end
        end
    end
  end
end

let rec solve_constraints = begin fun inf ->
  begin fun pos ->
    begin fun cstrs ->
      (((YzList.fold_left ( [] )) cstrs) begin fun preds ->
        begin fun ((tc, t), inst_ref) ->
          begin try
            begin let insts = ((search_typeclasses inf) tc) in
            begin match t.Type.raw with
              | ((Type.Con typector) | (Type.App (typector, _))) ->
                begin try
                  begin let inst = ((List.assoc typector) insts) in
                  begin
                  ((( := ) inst_ref) inst);
                  preds
                  end
                  end
                with
                  | Not_found ->
                    (failwith (((instance_not_found pos) tc) t))
                end
              | ((Type.Tuple _) | (Type.Fun (_, _))) ->
                (failwith (((instance_not_found pos) tc) t))
              | (Type.Var (_, _)) ->
                (( :: ) ((tc, t), preds))
              | (Type.Gen _) ->
                (assert false)
            end
            end
          with
            | Not_found ->
              (failwith ((unbound_type_class pos) tc))
          end
        end
      end)
    end
  end
end

let rec infer_expr = begin fun inf ->
  begin fun cstrs ->
    begin fun expr ->
      begin match expr.Expr.raw with
        | (Expr.Con lit) ->
          (((Type.at (Some expr.Expr.pos)) (infer_literal lit)), cstrs)
        | (Expr.Var (path, insts)) ->
          begin try
            begin let scm = ((search_asp inf) path) in
            begin let (preds, t) = ((instantiate inf.let_level) scm) in
            begin let (insts, cstrs) = (((YzList.fold_right preds) (( [] ), cstrs)) begin fun pred ->
              begin fun (insts, cstrs) ->
                begin let inst = (ref ()) in
                begin let cstr = (pred, inst) in
                ((( :: ) (inst, insts)), (( :: ) (cstr, cstrs)))
                end
                end
              end
            end) in
            (t, cstrs)
            end
            end
            end
          with
            | Not_found ->
              (failwith ((unbound_variable expr.Expr.pos) path))
          end
        | (Expr.Abs (pat, body_expr)) ->
          begin let (inf, pat_type, map) = ((infer_pattern inf) pat) in
          begin let (body_type, cstrs) = (((infer_expr inf) cstrs) body_expr) in
          (((Type.at (Some expr.Expr.pos)) (Type.Fun (pat_type, body_type))), cstrs)
          end
          end
        | (Expr.App (fun_expr, arg_expr)) ->
          begin let (fun_type, cstrs) = (((infer_expr inf) cstrs) fun_expr) in
          begin let (arg_type, cstrs) = (((infer_expr inf) cstrs) arg_expr) in
          (((((apply inf.let_level) expr.Expr.pos) fun_type) arg_type), cstrs)
          end
          end
        | (Expr.Ctor (ctor, opt_arg_expr)) ->
          begin try
            begin match (((search_ctors inf) ctor), opt_arg_expr) with
              | ((false, scm), None) ->
                begin let (preds0, ctor_type) = ((instantiate inf.let_level) scm) in
                begin
                (assert ((( = ) preds0) ( [] )));
                (ctor_type, cstrs)
                end
                end
              | ((false, scm), (Some arg_expr)) ->
                (failwith (((wrong_number_of_arguments expr.Expr.pos) 1) 0))
              | ((true, scm), None) ->
                (failwith (((wrong_number_of_arguments expr.Expr.pos) 0) 1))
              | ((true, scm), (Some arg_expr)) ->
                begin let (arg_type, cstrs) = (((infer_expr inf) cstrs) arg_expr) in
                begin let (preds0, ctor_type) = ((instantiate inf.let_level) scm) in
                begin
                (assert ((( = ) preds0) ( [] )));
                (((((apply inf.let_level) expr.Expr.pos) ctor_type) arg_type), cstrs)
                end
                end
                end
            end
          with
            | Not_found ->
              (failwith ((unbound_constructor expr.Expr.pos) ctor))
          end
        | (Expr.If (cond_expr, then_expr, else_expr)) ->
          begin let (cond_type, cstrs) = (((infer_expr inf) cstrs) cond_expr) in
          begin let (then_type, cstrs) = (((infer_expr inf) cstrs) then_expr) in
          begin let (else_type, cstrs) = (((infer_expr inf) cstrs) else_expr) in
          begin
          (((require cond_expr.Expr.pos) ((Type.at None) bool_type)) cond_type);
          begin
          begin try
            ((Type.unify then_type) else_type)
          with
            | (Type.Unification_error (t1, t2)) ->
              (failwith (((((invalid_if_expr expr.Expr.pos) then_type) else_type) t1) t2))
          end;
          (else_type, cstrs)
          end
          end
          end
          end
          end
        | (Expr.Tuple exprs) ->
          begin let (ts, cstrs) = (((YzList.fold_right exprs) (( [] ), cstrs)) begin fun expr ->
            begin fun (ts, cstrs) ->
              begin let (t, cstrs) = (((infer_expr inf) cstrs) expr) in
              ((( :: ) (t, ts)), cstrs)
              end
            end
          end) in
          (((Type.at (Some expr.Expr.pos)) (Type.Tuple ts)), cstrs)
          end
        | (Expr.Or (lhs, rhs)) ->
          begin let pos = expr.Expr.pos in
          begin let or_op = ((Expr.at pos) (Expr.Var ((( [] ), (Names.Op "||")), (ref ())))) in
          begin let or_expr = ((Expr.at pos) (Expr.App (((Expr.at pos) (Expr.App (or_op, lhs))), rhs))) in
          (((infer_expr inf) cstrs) or_expr)
          end
          end
          end
        | (Expr.And (lhs, rhs)) ->
          begin let pos = expr.Expr.pos in
          begin let and_op = ((Expr.at pos) (Expr.Var ((( [] ), (Names.Op "&&")), (ref ())))) in
          begin let and_expr = ((Expr.at pos) (Expr.App (((Expr.at pos) (Expr.App (and_op, lhs))), rhs))) in
          (((infer_expr inf) cstrs) and_expr)
          end
          end
          end
        | (Expr.Seq (lhs, rhs)) ->
          begin let (lhs_type, cstrs) = (((infer_expr inf) cstrs) lhs) in
          begin let (rhs_type, cstrs) = (((infer_expr inf) cstrs) rhs) in
          begin
          (((require lhs.Expr.pos) ((Type.at None) unit_type)) lhs_type);
          (rhs_type, cstrs)
          end
          end
          end
        | (Expr.LetVal (pat, val_expr, cont_expr)) ->
          begin let (inf, cstrs, map) = ((((infer_let_val inf) cstrs) pat) val_expr) in
          (((infer_expr inf) cstrs) cont_expr)
          end
        | (Expr.LetFun (defs, cont_expr)) ->
          begin let (inf, decls) = ((infer_let_fun inf) defs) in
          (((infer_expr inf) cstrs) cont_expr)
          end
        | (Expr.Match (target_expr, cases)) ->
          begin let (target_type, cstrs) = (((infer_expr inf) cstrs) target_expr) in
          begin let ret_type = (Type.make_var inf.let_level) in
          (((((infer_cases inf) cstrs) target_type) ret_type) cases)
          end
          end
        | (Expr.Try (expr, cases)) ->
          begin let target_type = ((Type.at None) exn_type) in
          begin let (ret_type, cstrs) = (((infer_expr inf) cstrs) expr) in
          (((((infer_cases inf) cstrs) target_type) ret_type) cases)
          end
          end
        | (Expr.Field (record_expr, path)) ->
          begin let (record_type, cstrs) = (((infer_expr inf) cstrs) record_expr) in
          begin try
            begin let (is_mutable, access_fun_scm) = ((search_fields inf) path) in
            begin let (preds0, access_fun_type) = ((instantiate inf.let_level) access_fun_scm) in
            begin
            (assert ((( = ) preds0) ( [] )));
            (((((apply inf.let_level) expr.Expr.pos) access_fun_type) record_type), cstrs)
            end
            end
            end
          with
            | Not_found ->
              (failwith ((unbound_field_label expr.Expr.pos) path))
          end
          end
        | (Expr.Assign (record_expr, path, val_expr)) ->
          begin let (record_type, cstrs) = (((infer_expr inf) cstrs) record_expr) in
          begin let (val_type, cstrs) = (((infer_expr inf) cstrs) val_expr) in
          begin try
            begin let (is_mutable, access_fun_scm) = ((search_fields inf) path) in
            begin if (not is_mutable) then
              (failwith ((field_not_mutable expr.Expr.pos) path))
            else
              begin let (preds0, access_fun_type) = ((instantiate inf.let_level) access_fun_scm) in
              begin
              (assert ((( = ) preds0) ( [] )));
              begin let field_type = ((((apply inf.let_level) expr.Expr.pos) access_fun_type) record_type) in
              begin
              (((require val_expr.Expr.pos) field_type) val_type);
              (((Type.at (Some expr.Expr.pos)) unit_type), cstrs)
              end
              end
              end
              end
            end
            end
          with
            | Not_found ->
              (failwith ((unbound_field_label expr.Expr.pos) path))
          end
          end
          end
        | (Expr.Record field_defs) ->
          begin let record_type = (Type.make_var inf.let_level) in
          (((((infer_field_defs inf) cstrs) expr.Expr.pos) record_type) field_defs)
          end
        | (Expr.Update (record_expr, field_defs)) ->
          begin let (record_type, cstrs) = (((infer_expr inf) cstrs) record_expr) in
          (((((infer_field_defs inf) cstrs) expr.Expr.pos) record_type) field_defs)
          end
      end
    end
  end
end

and infer_let_val = begin fun inf ->
  begin fun cstrs ->
    begin fun pat ->
      begin fun val_expr ->
        begin let (val_type, cstrs) = (((infer_expr inf) cstrs) val_expr) in
        begin let (inf, pat_type, map) = ((infer_pattern inf) pat) in
        begin
        (((require val_expr.Expr.pos) pat_type) val_type);
        (inf, cstrs, map)
        end
        end
        end
      end
    end
  end
end

and infer_let_fun = begin fun inf ->
  begin fun defs ->
    begin let init = (inf, ( [] )) in
    begin let (tmp_inf, defs) = (((YzList.fold_left init) defs) begin fun (tmp_inf, defs) ->
      begin fun (name, val_expr) ->
        begin let (tmp_inf, t) = ((add_type_var tmp_inf) name) in
        (tmp_inf, (( :: ) ((name, val_expr, t), defs)))
        end
      end
    end) in
    begin let let_level = tmp_inf.let_level in
    begin let tmp_inf = (incr_let_level tmp_inf) in
    (((YzList.fold_left (inf, ( [] ))) defs) begin fun (inf, decls) ->
      begin fun (name, val_expr, type_var) ->
        begin let (val_type, cstrs) = (((infer_expr tmp_inf) ( [] )) val_expr) in
        begin
        (((require val_expr.Expr.pos) type_var) val_type);
        begin let preds = (((solve_constraints inf) val_expr.Expr.pos) cstrs) in
        begin let scm = (((generalize let_level) preds) val_type) in
        ((((add_asp inf) name) scm), (( :: ) ((Decl.Val (name, scm)), decls)))
        end
        end
        end
        end
      end
    end)
    end
    end
    end
    end
  end
end

and infer_cases = begin fun inf ->
  begin fun cstrs ->
    begin fun target_type ->
      begin fun ret_type ->
        begin fun cases ->
          begin let cstrs = (((YzList.fold_left cstrs) cases) begin fun cstrs ->
            begin fun (pat, opt_guard, body_expr) ->
              begin let (inf, pat_type, map) = ((infer_pattern inf) pat) in
              begin
              (((require pat.Pattern.pos) target_type) pat_type);
              begin
              begin match opt_guard with
                | None ->
                  ()
                | (Some guard) ->
                  begin let (guard_type, cstrs) = (((infer_expr inf) cstrs) guard) in
                  (((require guard.Expr.pos) ((Type.at None) bool_type)) guard_type)
                  end
              end;
              begin let (body_type, cstrs) = (((infer_expr inf) cstrs) body_expr) in
              begin
              (((require body_expr.Expr.pos) ret_type) body_type);
              cstrs
              end
              end
              end
              end
              end
            end
          end) in
          (ret_type, cstrs)
          end
        end
      end
    end
  end
end

and infer_field_defs = begin fun inf ->
  begin fun cstrs ->
    begin fun pos ->
      begin fun record_type ->
        begin fun field_defs ->
          begin let cstrs = (((YzList.fold_left cstrs) field_defs) begin fun cstrs ->
            begin fun (path, val_expr) ->
              begin try
                begin let (is_mutable, access_fun_scm) = ((search_fields inf) path) in
                begin let (preds0, access_fun_type) = ((instantiate inf.let_level) access_fun_scm) in
                begin
                (assert ((( = ) preds0) ( [] )));
                begin let field_type = ((((apply inf.let_level) pos) access_fun_type) record_type) in
                begin let (val_type, cstrs) = (((infer_expr inf) cstrs) val_expr) in
                begin
                (((require val_expr.Expr.pos) field_type) val_type);
                cstrs
                end
                end
                end
                end
                end
                end
              with
                | Not_found ->
                  (failwith ((unbound_field_label pos) path))
              end
            end
          end) in
          (record_type, cstrs)
          end
        end
      end
    end
  end
end

and require = begin fun pos ->
  begin fun req_type ->
    begin fun got_type ->
      begin try
        ((Type.unify req_type) got_type)
      with
        | (Type.Unification_error (t1, t2)) ->
          (failwith (((((required pos) req_type) got_type) t1) t2))
      end
    end
  end
end

let rec make_decls = begin fun map ->
  (List.rev (((ValNameMap.fold begin fun name ->
    begin fun t ->
      begin fun acc ->
        (( :: ) ((Decl.Val (name, (Scheme.mono t))), acc))
      end
    end
  end) map) ( [] )))
end

let rec expand_abbrev = begin fun inf ->
  begin fun pos ->
    begin fun t ->
      begin fun opt_conv ->
        begin match opt_conv with
          | None ->
            t
          | (Some conv_fun_scm) ->
            begin let (preds0, conv_fun_type) = ((instantiate inf.let_level) conv_fun_scm) in
            begin
            (assert ((( = ) preds0) ( [] )));
            ((((apply inf.let_level) pos) conv_fun_type) t)
            end
            end
        end
      end
    end
  end
end

let rec eval = begin fun inf ->
  begin fun env_ref ->
    begin fun type_expr ->
      begin match type_expr.TypeExpr.raw with
        | (TypeExpr.Con typector) ->
          begin try
            begin let (typector, param_num, opt_conv) = ((search_typectors inf) typector) in
            begin if ((( = ) param_num) 0) then
              begin let t = ((Type.at (Some type_expr.TypeExpr.pos)) (Type.Con typector)) in
              ((((expand_abbrev inf) type_expr.TypeExpr.pos) t) opt_conv)
              end
            else
              (failwith (((wrong_number_of_arguments type_expr.TypeExpr.pos) 0) param_num))
            end
            end
          with
            | Not_found ->
              (failwith ((unbound_type_constructor type_expr.TypeExpr.pos) typector))
          end
        | (TypeExpr.Var name) ->
          begin try
            ((List.assoc name) (( ! ) env_ref))
          with
            | Not_found ->
              begin let t = (Type.make_var inf.let_level) in
              begin
              ((( := ) env_ref) (( :: ) ((name, t), (( ! ) env_ref))));
              t
              end
              end
          end
        | (TypeExpr.App (typector, ts)) ->
          begin try
            begin let (typector, param_num, opt_conv) = ((search_typectors inf) typector) in
            begin let arg_num = (List.length ts) in
            begin if ((( = ) param_num) arg_num) then
              begin let ts = ((List.map ((eval inf) env_ref)) ts) in
              begin let t = ((Type.at (Some type_expr.TypeExpr.pos)) (Type.App (typector, ts))) in
              ((((expand_abbrev inf) type_expr.TypeExpr.pos) t) opt_conv)
              end
              end
            else
              (failwith (((wrong_number_of_arguments type_expr.TypeExpr.pos) arg_num) param_num))
            end
            end
            end
          with
            | Not_found ->
              (failwith ((unbound_type_constructor type_expr.TypeExpr.pos) typector))
          end
        | (TypeExpr.Tuple ts) ->
          ((Type.at (Some type_expr.TypeExpr.pos)) (Type.Tuple ((List.map ((eval inf) env_ref)) ts)))
        | (TypeExpr.Fun (t1, t2)) ->
          begin let t10 = (((eval inf) env_ref) t1) in
          begin let t20 = (((eval inf) env_ref) t2) in
          ((Type.at (Some type_expr.TypeExpr.pos)) (Type.Fun (t10, t20)))
          end
          end
      end
    end
  end
end

let rec load_type_info = begin fun inf ->
  begin fun type_info ->
    begin match type_info with
      | (TypeInfo.Variant ctor_decls) ->
        begin let let_level = inf.let_level in
        begin let tmp_inf = (incr_let_level inf) in
        (((YzList.fold_left inf) ctor_decls) begin fun inf ->
          begin fun (ctor_name, opt_param, ctor_type_expr) ->
            begin let ctor_type = (((eval tmp_inf) (ref ( [] ))) ctor_type_expr) in
            begin let ctor_scm = (((generalize let_level) ( [] )) ctor_type) in
            begin match opt_param with
              | None ->
                (((add_ctor inf) ctor_name) (false, ctor_scm))
              | (Some _) ->
                (((add_ctor inf) ctor_name) (true, ctor_scm))
            end
            end
            end
          end
        end)
        end
        end
      | (TypeInfo.Record field_decls) ->
        begin let let_level = inf.let_level in
        begin let tmp_inf = (incr_let_level inf) in
        (((YzList.fold_left inf) field_decls) begin fun inf ->
          begin fun (is_mutable, field_name, _, access_type_expr) ->
            begin let access_type = (((eval tmp_inf) (ref ( [] ))) access_type_expr) in
            begin let access_scm = (((generalize let_level) ( [] )) access_type) in
            (((add_field inf) field_name) (is_mutable, access_scm))
            end
            end
          end
        end)
        end
        end
    end
  end
end

let rec load_exn_decl = begin fun inf ->
  begin fun (ctor_name, opt_param, ctor_type_expr) ->
    begin let let_level = inf.let_level in
    begin let tmp_inf = (incr_let_level inf) in
    begin let ctor_type = (((eval tmp_inf) (ref ( [] ))) ctor_type_expr) in
    begin let ctor_scm = (((generalize let_level) ( [] )) ctor_type) in
    begin match opt_param with
      | None ->
        (((add_ctor inf) ctor_name) (false, ctor_scm))
      | (Some _) ->
        (((add_ctor inf) ctor_name) (true, ctor_scm))
    end
    end
    end
    end
    end
  end
end

let rec load_type_defs = begin fun inf ->
  begin fun defs ->
    begin let inf = (((YzList.fold_left inf) defs) begin fun inf ->
      begin fun type_def ->
        begin match type_def with
          | ((TypeDef.Repr (name, type_params, _)) | (TypeDef.Abbrev (name, type_params, _, _))) ->
            begin let typector = ((( :: ) (inf.mod_name, ( [] ))), name) in
            begin let param_num = (List.length type_params) in
            (((add_typector inf) name) (typector, param_num, None))
            end
            end
        end
      end
    end) in
    (((YzList.fold_left inf) defs) begin fun inf ->
      begin fun type_def ->
        begin match type_def with
          | (TypeDef.Repr (name, type_params, type_info)) ->
            ((load_type_info inf) type_info)
          | (TypeDef.Abbrev (name, type_params, _, conv_type_expr)) ->
            begin let let_level = inf.let_level in
            begin let tmp_inf = (incr_let_level inf) in
            begin let conv_type = (((eval tmp_inf) (ref ( [] ))) conv_type_expr) in
            begin let conv_scm = (((generalize let_level) ( [] )) conv_type) in
            begin let typector = ((( :: ) (inf.mod_name, ( [] ))), name) in
            begin let param_num = (List.length type_params) in
            (((add_typector inf) name) (typector, param_num, (Some conv_scm)))
            end
            end
            end
            end
            end
            end
        end
      end
    end)
    end
  end
end

let rec infer_top = begin fun inf ->
  begin fun top ->
    begin match top.Top.raw with
      | (Top.Expr expr) ->
        begin let (t, cstrs) = (((infer_expr inf) ( [] )) expr) in
        (inf, (( :: ) ((Decl.Val ((Names.Id "_"), (Scheme.mono t))), ( [] ))))
        end
      | (Top.LetVal (pat, val_expr)) ->
        begin let (inf, cstrs, map) = ((((infer_let_val inf) ( [] )) pat) val_expr) in
        (inf, (make_decls map))
        end
      | (Top.LetFun defs) ->
        begin let (inf, decls) = ((infer_let_fun inf) defs) in
        (inf, decls)
        end
      | (Top.Type defs) ->
        begin let inf = ((load_type_defs inf) defs) in
        (inf, ( [] ))
        end
      | (Top.Open (( :: ) (mod_name, mod_path))) ->
        begin let inf = {
          inf with
          opens = (( :: ) ((mod_name, mod_path), inf.opens));
        } in
        (inf, ( [] ))
        end
      | (Top.Open ( [] )) ->
        (assert false)
      | (Top.Exception exn_decl) ->
        begin let inf = ((load_exn_decl inf) exn_decl) in
        (inf, ( [] ))
        end
    end
  end
end

let rec load_decl = begin fun inf ->
  begin fun decl ->
    begin match decl with
      | (DeclExpr.Val (name, type_expr)) ->
        begin let let_level = inf.let_level in
        begin let tmp_inf = (incr_let_level inf) in
        begin let t = (((eval tmp_inf) (ref ( [] ))) type_expr) in
        begin let scm = (((generalize let_level) ( [] )) t) in
        (((add_asp inf) name) scm)
        end
        end
        end
        end
      | (DeclExpr.AbstrType (name, param_num)) ->
        begin let typector = ((( :: ) (inf.mod_name, ( [] ))), name) in
        (((add_typector inf) name) (typector, param_num, None))
        end
      | (DeclExpr.ConcrType defs) ->
        ((load_type_defs inf) defs)
      | (DeclExpr.Exception exn_decl) ->
        ((load_exn_decl inf) exn_decl)
    end
  end
end

let rec enter_module = begin fun inf ->
  begin fun mod_name ->
    {
      inf with
      mod_name = mod_name;
    }
  end
end

let rec leave_module = begin fun inf ->
  begin fun mod_name ->
    (create (( :: ) ((mod_name, inf.curr_mod), inf.mods)))
  end
end

