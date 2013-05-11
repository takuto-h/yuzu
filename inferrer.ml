
open Type.Open
open Printf
  
type t = {
  asp : (Ident.t * Scheme.t) list;
  let_level : int;
}

let empty = {asp=[];let_level=0}
    
let make_type_var let_level =
  Type.Var(let_level,ref None)

let generalize let_level t =
  let alist = ref [] in
  let rec gen t =
    begin match t with
      | Type.Con(_,_) ->
        t
      | Type.Var(lv,tref) ->
        begin match !tref with
          | Some(tt) ->
            gen tt
          | None when lv > let_level ->
            begin try
              List.assq tref !alist
            with
              | Not_found ->
                let tgen = Type.Gen(List.length !alist) in begin
                alist := (tref,tgen)::!alist;
                tgen
                end
            end
          | None ->
            t
        end
      | Type.Gen(_) ->
        assert false
      | Type.App(t1,t2) ->
        Type.App(gen t1,gen t2)
    end
  in
  Scheme.poly (List.length !alist) (gen t)

let instantiate let_level {Scheme.gen_num;Scheme.body} =
  let type_vars = Array.init gen_num (fun _ -> make_type_var let_level) in
  let rec inst t =
    begin match t with
      | Type.Con(_,_) -> t
      | Type.Var(_,_) -> t
      | Type.Gen(n) -> Array.get type_vars n
      | Type.App(t1,t2) -> Type.App(inst t1,inst t2)
    end
  in
  inst body

let rec infer_expr inf expr =
  begin match expr.Expr.raw with
    | Expr.Con(lit) ->
      begin match lit with
        | Literal.Unit -> Type.Con(expr.Expr.pos, Ident.intern "()")
        | Literal.Int(_) -> Type.Con(expr.Expr.pos, Ident.intern "Int")
        | Literal.Bool(_) -> Type.Con(expr.Expr.pos, Ident.intern "Bool")
      end
    | Expr.Var(ident) ->
      begin try
        instantiate inf.let_level (List.assoc ident inf.asp)
      with
        | Not_found ->
          failwith
            (sprintf
               "%s: error: unbound variable: %s\n%s"
               (Pos.show expr.Expr.pos) (Ident.show ident) (Pos.show_source expr.Expr.pos))
      end
    | Expr.Abs(para_ident,body_expr) ->
      let para_type = make_type_var inf.let_level in
      let inf = {inf with asp=(para_ident,Scheme.mono para_type)::inf.asp} in
      let body_type = infer_expr inf body_expr in
      (para_type @-> body_type) expr.Expr.pos
    | Expr.App(fun_expr,arg_expr) ->
      let fun_type = infer_expr inf fun_expr in
      let arg_type = infer_expr inf arg_expr in
      let ret_type = make_type_var inf.let_level in begin
      Type.unify expr.Expr.pos fun_type ((arg_type @-> ret_type) expr.Expr.pos);
      ret_type
      end
    | Expr.LetVal(ident,val_expr,body_expr) ->
      let val_type = infer_expr inf val_expr in
      let inf_body = {inf with asp=(ident,Scheme.mono val_type)::inf.asp} in
      infer_expr inf_body body_expr
    | Expr.LetFun(ident,fun_expr,body_expr) ->
      let let_level = inf.let_level in
      let fun_type_var = make_type_var (let_level+1) in
      let inf_fun = {inf with let_level=let_level+1} in
      let inf_fun = {inf_fun with asp=(ident,Scheme.mono fun_type_var)::inf.asp} in
      let fun_type = infer_expr inf_fun fun_expr in begin
      Type.unify expr.Expr.pos fun_type_var fun_type;
      let inf_body = {inf with asp=(ident,generalize let_level fun_type)::inf.asp} in
      infer_expr inf_body body_expr
      end
  end

let infer_top inf top =
  begin match top.Top.raw with
    | Top.LetVal(ident,val_expr) ->
      let val_scm = Scheme.mono (infer_expr inf val_expr) in
      let inf_body = {inf with asp=(ident,val_scm)::inf.asp} in
      (val_scm, inf_body)
    | Top.LetFun(ident,fun_expr) ->
      let let_level = inf.let_level in
      let fun_type_var = make_type_var (let_level+1) in
      let inf_fun = {inf with let_level=let_level+1} in
      let inf_fun = {inf_fun with asp=(ident,Scheme.mono fun_type_var)::inf.asp} in
      let fun_type = infer_expr inf_fun fun_expr in begin
      Type.unify top.Top.pos fun_type_var fun_type;
      let fun_scm = generalize let_level fun_type in
      let inf_body = {inf with asp=(ident,fun_scm)::inf.asp} in
      (fun_scm, inf_body)
      end
    | Top.Expr(expr) ->
      (generalize inf.let_level (infer_expr inf expr), inf)
  end
