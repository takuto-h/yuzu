open Printf

type t =
  | Con of Literal.t
  | Var of Names.val_path
  | Ctor of Names.ctor
  | Abs of (Pattern.t * t)
  | App of (t * t)
  | If of (t * t * t)
  | Tuple of (t) list
  | Record of ((Names.val_path * t)) list
  | Match of (t * ((Pattern.t * t)) list)
  | LetVal of (Pattern.t * t * t)
  | LetFun of (Names.val_name * t * t)
  | Seq of (t * t)

let rec show = begin fun expr ->
  begin match expr with
    | (Con(lit)) ->
      ((sprintf "Con(%s)") (Literal.show lit))
    | (Var(path)) ->
      ((sprintf "Var(%s)") (Names.show_val_path path))
    | (Ctor(ctor)) ->
      ((sprintf "Ctor(%s)") (Names.show_ctor ctor))
    | (Abs(param_pat, body_expr)) ->
      (((sprintf "Abs(%s,%s)") (Pattern.show param_pat)) (show body_expr))
    | (App(fun_expr, arg_expr)) ->
      (((sprintf "App(%s,%s)") (show fun_expr)) (show arg_expr))
    | (If(cond_expr, then_expr, else_expr)) ->
      ((((sprintf "If(%s,%s,%s)") (show cond_expr)) (show then_expr)) (show else_expr))
    | (Tuple((( :: )(x, xs)))) ->
      ((sprintf "Tuple([%s])") (((List.fold_left begin fun acc ->
        begin fun elem ->
          (((sprintf "%s;%s") acc) (show elem))
        end
      end) (show x)) xs))
    | (Tuple(([](_)))) ->
      (assert false)
    | (Record(field_defs)) ->
      (sprintf "Record()")
    | (Match(target_expr, ([](_)))) ->
      ((sprintf "Match(%s,[])") (show target_expr))
    | (Match(target_expr, (( :: )(c, cs)))) ->
      begin let rec show_case = begin fun (pat, expr) ->
        (((sprintf "Case(%s,%s)") (Pattern.show pat)) (show expr))
      end in
      (((sprintf "Match(%s,[%s])") (show target_expr)) (((List.fold_left begin fun acc ->
        begin fun elem ->
          (((sprintf "%s;%s") acc) (show_case elem))
        end
      end) (show_case c)) cs))
      end
    | (LetVal(pat, val_expr, cont_expr)) ->
      begin let str_pat = (Pattern.show pat) in
      begin let str_val = (show val_expr) in
      begin let str_cont = (show cont_expr) in
      ((((sprintf "LetVal(%s,%s,%s)") str_pat) str_val) str_cont)
      end
      end
      end
    | (LetFun(name, val_expr, cont_expr)) ->
      begin let str_name = (Names.show_val_name name) in
      begin let str_val = (show val_expr) in
      begin let str_cont = (show cont_expr) in
      ((((sprintf "LetFun(%s,%s,%s)") str_name) str_val) str_cont)
      end
      end
      end
    | (Seq(lhs, rhs)) ->
      begin let str_lhs = (show lhs) in
      begin let str_rhs = (show rhs) in
      (((sprintf "Seq(%s,%s)") str_lhs) str_rhs)
      end
      end
  end
end

