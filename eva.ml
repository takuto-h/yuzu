
type t = {env:Value.env; dummy:unit}

let empty = {env=[]; dummy=()}
    
let rec eval_expr eva expr =
  begin match expr with
    | Expr.Con(lit) ->
      Value.Con(lit)
    | Expr.Var(ident) ->
      begin try
        List.assoc ident eva.env
      with
        | Not_found -> failwith "unbound variable"
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
        | _ ->
          failwith "function required"
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
          failwith "function required"
      end
  end

let eval_top eva top =
  begin match top with
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
          failwith "function required"
      end
    | Top.Expr(expr) ->
      (eval_expr eva expr, eva)
  end