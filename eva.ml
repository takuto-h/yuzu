
open Printf

type t = {env:Value.env; dummy:unit}

let default_env = [
  Ident.intern("+"), Value.Subr begin fun arg1 ->
    Value.Subr begin fun arg2 ->
      begin match (arg1,arg2) with
        | (Value.Con(Literal.Int(n1)),Value.Con(Literal.Int(n2))) ->
          Value.Con(Literal.Int(n1 + n2))
        | _ ->
          assert false
      end
    end
  end;
  Ident.intern("-"), Value.Subr begin fun arg1 ->
    Value.Subr begin fun arg2 ->
      begin match (arg1,arg2) with
        | (Value.Con(Literal.Int(n1)),Value.Con(Literal.Int(n2))) ->
          Value.Con(Literal.Int(n1 - n2))
        | _ ->
          assert false
      end
    end
  end;
  Ident.intern("*"), Value.Subr begin fun arg1 ->
    Value.Subr begin fun arg2 ->
      begin match (arg1,arg2) with
        | (Value.Con(Literal.Int(n1)),Value.Con(Literal.Int(n2))) ->
          Value.Con(Literal.Int(n1 * n2))
        | _ ->
          assert false
      end
    end
  end;
  Ident.intern("=="), Value.Subr begin fun arg1 ->
    Value.Subr begin fun arg2 ->
      begin match (arg1,arg2) with
        | (Value.Con(Literal.Int(n1)),Value.Con(Literal.Int(n2))) ->
          Value.Con(Literal.Bool(n1 = n2))
        | _ ->
          assert false
      end
    end
  end
]
  
let default = {env=default_env; dummy=()}
    
let rec eval_expr eva expr =
  begin match expr.Expr.raw with
    | Expr.Con(lit) ->
      Value.Con(lit)
    | Expr.Var(ident) ->
      begin try
        List.assoc ident eva.env
      with
        | Not_found ->
          failwith
            (sprintf
               "%s: RUNTIME ERROR: unbound variable: %s\n%s"
               (Pos.show expr.Expr.pos) (Ident.show ident) (Pos.show_source expr.Expr.pos))
      end
    | Expr.Abs(para_ident,body_expr) ->
      Value.Closure(ref eva.env, para_ident, body_expr)
    | Expr.App(fun_expr,arg_expr) ->
      let fun_val = eval_expr eva fun_expr in
      let arg_val = eval_expr eva arg_expr in
      begin match fun_val with
        | Value.Closure(envref,para_ident,body_expr) ->
          let eva_body = {eva with env=(para_ident,arg_val)::!envref} in
          eval_expr eva_body body_expr
        | Value.Subr(subr) ->
          subr arg_val
        | _ ->
          failwith
            (sprintf
               "%s: RUNTIME ERROR: function required, but got: %s\n%s"
               (Pos.show fun_expr.Expr.pos)
               (Value.show fun_val)
               (Pos.show_source fun_expr.Expr.pos))
      end
    | Expr.LetVal(ident,val_expr,body_expr) ->
      let val_val = eval_expr eva val_expr in
      let eva_body = {eva with env=(ident,val_val)::eva.env} in
      eval_expr eva_body body_expr
    | Expr.LetFun(ident,fun_expr,body_expr) ->
      let fun_val = eval_expr eva fun_expr in
      begin match fun_val with
        | Value.Closure(envref,_,_) -> begin
          envref := (ident,fun_val)::!envref;
          let eva_body = {eva with env=(ident,fun_val)::eva.env} in
          eval_expr eva_body body_expr
        end
        | _ ->
          failwith
            (sprintf
               "%s: RUNTIME ERROR: function required, but got: %s\n%s"
               (Pos.show fun_expr.Expr.pos)
               (Value.show fun_val)
               (Pos.show_source fun_expr.Expr.pos))
      end
    | Expr.If(cond_expr,then_expr,else_expr) ->
      let cond_val = eval_expr eva cond_expr in
      begin match cond_val with
        | Value.Con(Literal.Bool(true)) ->
          eval_expr eva then_expr
        | Value.Con(Literal.Bool(false)) ->
          eval_expr eva else_expr
        | _ ->
          failwith
            (sprintf
               "%s: RUNTIME ERROR: boolean required, but got: %s\n%s"
               (Pos.show expr.Expr.pos)
               (Value.show cond_val)
               (Pos.show_source expr.Expr.pos))
      end
    | Expr.Tuple(lst) ->
      Value.Tuple(List.map (eval_expr eva) lst)
    | Expr.LetTuple(ident_list, val_expr, body_expr) ->
      let val_val = eval_expr eva val_expr in
      begin match val_val with
        | Value.Tuple(val_list) ->
          let ident_val_env = List.map2 (fun k v -> (k,v)) ident_list val_list in
          let eva_body = {eva with env=List.append ident_val_env eva.env} in
          eval_expr eva_body body_expr
        | _ ->
          failwith
            (sprintf
               "%s: RUNTIME ERROR: tuple required, but got: %s\n%s"
               (Pos.show expr.Expr.pos)
               (Value.show val_val)
               (Pos.show_source expr.Expr.pos))
      end
  end

let eval_top eva top =
  begin match top.Top.raw with
    | Top.LetVal(ident,val_expr) ->
      let val_val = eval_expr eva val_expr in
      let eva_body = {eva with env=(ident,val_val)::eva.env} in
      (val_val, eva_body)
    | Top.LetFun(ident,fun_expr) ->
      let fun_val = eval_expr eva fun_expr in
      begin match fun_val with
        | Value.Closure(envref,_,_) -> begin
          envref := (ident,fun_val)::!envref;
          let eva_body = {eva with env=(ident,fun_val)::eva.env} in
          (fun_val, eva_body)
        end
        | _ ->
          failwith
            (sprintf
               "%s: RUNTIME ERROR: function required, but got: %s\n%s"
               (Pos.show fun_expr.Expr.pos)
               (Value.show fun_val)
               (Pos.show_source fun_expr.Expr.pos))
      end
    | Top.Expr(expr) ->
      (eval_expr eva expr, eva)
    | Top.Type(_) ->
      (Value.Con(Literal.Unit), eva)
  end
