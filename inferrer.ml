
open Type.Open

type t = {
  asp : (Ident.t * Scheme.t) list;
  let_level : int;
}

let make_type_var let_level =
  Type.Var(let_level,ref None)

let generalize let_level t =
  let table = Hashtbl.create 11 in
  let rec gen t =
    begin match t with
      | Type.Con(_) ->
        t
      | Type.Var(lv,tref) ->
        begin match !tref with
          | Some(tt) ->
            gen tt
          | None when lv > let_level -> begin
            try
              Hashtbl.find table tref
            with
              | Not_found ->
                let tgen = Type.Gen(Hashtbl.length table) in begin
                Hashtbl.add table tref tgen;
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
  Scheme.poly (Hashtbl.length table) (gen t)

let instantiate let_level {Scheme.gen_num;Scheme.body} =
  let type_vars = Array.init gen_num (fun _ -> make_type_var let_level) in
  let rec inst t =
    begin match t with
      | Type.Con(_) -> t
      | Type.Var(_) -> t
      | Type.Gen(n) -> Array.get type_vars n
      | Type.App(t1,t2) -> Type.App(inst t1,inst t2)
    end
  in
  inst body

let rec infer_expr inf expr =
  begin match expr with
    | Expr.Con(lit) ->
      Literal.type_of_literal lit
    | Expr.Var(ident) ->
      begin try
        instantiate inf.let_level (List.assoc ident inf.asp)
      with
        | Not_found -> failwith "unbound variable"
      end
    | Expr.Abs(para_ident,body_expr) ->
      let para_type = make_type_var inf.let_level in
      let inf = {inf with asp=(para_ident,Scheme.mono para_type)::inf.asp} in
      let body_type = infer_expr inf body_expr in
      para_type @-> body_type
    | Expr.App(fun_expr,arg_expr) ->
      let fun_type = infer_expr inf fun_expr in
      let arg_type = infer_expr inf arg_expr in
      let ret_type = make_type_var inf.let_level in begin
      Type.unify fun_type (arg_type @-> ret_type);
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
      Type.unify fun_type_var fun_type;
      let inf_body = {inf with asp=(ident,generalize let_level fun_type)::inf.asp} in
      infer_expr inf_body body_expr
      end
  end

let infer_top inf top =
  begin match top with
    | Top.Expr(expr) -> generalize inf.let_level (infer_expr inf expr)
  end
    
let x = Ident.intern "x"
let e123 = Expr.Con(Literal.Int(123))
let eid = Expr.Abs(x,Expr.Var(x))
let eapp = Expr.App(eid,e123)
let inf = {asp=[]; let_level=0}
let id = Ident.intern "id"
let elet1 = Expr.LetFun(id,eid,eapp)
let elet2 = Expr.LetFun(id,eid,Expr.App(eid,eid))
