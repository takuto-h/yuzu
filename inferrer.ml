
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
        let t11 = gen t1 in
        let t22 = gen t2 in
        Type.App(t11,t22)
      | Type.Tuple(pos,lst) ->
        Type.Tuple(pos,List.map gen lst)
    end
  in
  Scheme.poly (List.length !alist) (gen t)

let instantiate let_level {Scheme.gen_num;Scheme.body} =
  let type_vars = Array.init gen_num (fun _ -> make_type_var let_level) in
  let rec inst t =
    begin match t with
      | Type.Con(_,_) -> t
      | Type.Var(_,tref) ->
        begin match !tref with
          | None -> t
          | Some(tt) -> inst tt
        end
      | Type.Gen(n) ->
        Array.get type_vars n
      | Type.App(t1,t2) ->
        Type.App(inst t1,inst t2)
      | Type.Tuple(pos,lst) ->
        Type.Tuple(pos,List.map inst lst)
    end
  in
  inst body

let invalid_app pos fun_type arg_type t1 t2 =
  let alist_ref = ref [] in
  let array = Array.make 0 "" in
  let str_t1 = Type.show alist_ref array t1 in
  let str_t2 = Type.show alist_ref array t2 in
  let str_fun_type = Type.show alist_ref array fun_type in
  let str_arg_type = Type.show alist_ref array arg_type in
  sprintf
    "%s: error: invalid application\n%s%s%s%s%s"
    (Pos.show pos)
    (sprintf "function type: %s\n" str_fun_type)
    (sprintf "argument type: %s\n" str_arg_type)
    (Pos.show_source pos)
    (Type.show_origin str_t1 t1) (Type.show_origin str_t2 t2)

let invalid_def pos ident fun_type_var fun_type t1 t2 =
  let alist_ref = ref [] in
  let array = Array.make 0 "" in
  let str_t1 = Type.show alist_ref array t1 in
  let str_t2 = Type.show alist_ref array t2 in
  let str_fun_type_var = Type.show alist_ref array fun_type_var in
  let str_fun_type = Type.show alist_ref array fun_type in
  sprintf
    "%s: error: invalid definition: %s\n%s%s%s%s%s"
    (Pos.show pos) (Ident.show ident)
    (sprintf "variable type: %s\n" str_fun_type_var)
    (sprintf "expression type: %s\n" str_fun_type)
    (Pos.show_source pos)
    (Type.show_origin str_t1 t1) (Type.show_origin str_t2 t2)

let required pos req got t2 =
  let alist_ref = ref [] in
  let array = Array.make 0 "" in
  let str_req = Type.show alist_ref array req in
  let str_got = Type.show alist_ref array got in
  let str_t2 = Type.show alist_ref array t2 in
  sprintf
    "%s: error: %s required, but got %s\n%s%s"
    (Pos.show pos) str_req str_got (Pos.show_source pos)
    (Type.show_origin str_t2 t2)

let invalid_if_expr pos then_type else_type t1 t2 =
  let alist_ref = ref [] in
  let array = Array.make 0 "" in
  let str_then_type = Type.show alist_ref array then_type in
  let str_else_type = Type.show alist_ref array else_type in
  let str_t1 = Type.show alist_ref array t1 in
  let str_t2 = Type.show alist_ref array t2 in
  sprintf
    "%s: error: invalid if expression\n%s%s%s%s%s"
    (Pos.show pos)
    (sprintf "type of then clause: %s\n" str_then_type)
    (sprintf "type of else clause: %s\n" str_else_type)
    (Pos.show_source pos) (Type.show_origin str_t1 t1) (Type.show_origin str_t2 t2)
    
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
      begin try
        Type.unify fun_type ((arg_type @-> ret_type) expr.Expr.pos);
        with
        | Type.Unification_failed(t1,t2) ->
          failwith (invalid_app expr.Expr.pos fun_type arg_type t1 t2)
      end;
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
      begin try
        Type.unify fun_type_var fun_type
      with
        | Type.Unification_failed(t1,t2) ->
          failwith (invalid_def expr.Expr.pos ident fun_type_var fun_type t1 t2)
      end;
      let inf_body = {inf with asp=(ident,generalize let_level fun_type)::inf.asp} in
      infer_expr inf_body body_expr
      end
    | Expr.If(cond_expr,then_expr,else_expr) ->
      let cond_type = infer_expr inf cond_expr in
      let then_type = infer_expr inf then_expr in
      let else_type = infer_expr inf else_expr in
      let bool_type = Type.Con(expr.Expr.pos,Ident.intern("Bool")) in begin
      begin try
        Type.unify bool_type cond_type
      with
        | Type.Unification_failed(_,t2) ->
          failwith (required expr.Expr.pos bool_type cond_type t2)
      end;
      begin try
        Type.unify then_type else_type
      with
        | Type.Unification_failed(t1,t2) ->
          failwith (invalid_if_expr expr.Expr.pos then_type else_type t1 t2)
      end;
      then_type
      end
    | Expr.Tuple(lst) ->
      Type.Tuple(expr.Expr.pos, List.map (infer_expr inf) lst)
    | Expr.LetTuple(ident_list, val_expr, body_expr) ->
      let val_type = infer_expr inf val_expr in
      let type_var_list = List.map (fun _ -> make_type_var inf.let_level) ident_list in
      let tuple_type = Type.Tuple(expr.Expr.pos, type_var_list) in
      Type.unify tuple_type val_type;
      let ident_type_asp = List.map2 begin fun k v ->
        (k,Scheme.mono v)
      end ident_list type_var_list in
      let inf_body = {inf with asp=List.append ident_type_asp inf.asp} in
      infer_expr inf_body body_expr
  end

let make_ctor_asp ret_type ctor_decls =
  List.map begin fun (pos,ident,arg_types) ->
    let t = List.fold_right begin fun elem acc ->
      (elem @-> acc) pos
    end arg_types ret_type in
    (ident, Scheme.mono t)
  end ctor_decls 
    
let rec make_case_type pos type_var ret_type ctor_decls =
  List.fold_right begin fun (pos,_,arg_types) acc ->
    let t = List.fold_right begin fun elem acc ->
      (elem @-> acc) pos
    end arg_types type_var in
    (t @-> acc) pos
  end ctor_decls ((ret_type @-> type_var) pos)
    
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
      begin try
        Type.unify fun_type_var fun_type
      with
        | Type.Unification_failed(t1,t2) ->
          failwith (invalid_def top.Top.pos ident fun_type_var fun_type t1 t2)
      end;
      let fun_scm = generalize let_level fun_type in
      let inf_body = {inf with asp=(ident,fun_scm)::inf.asp} in
      (fun_scm, inf_body)
      end
    | Top.Expr(expr) ->
      (generalize inf.let_level (infer_expr inf expr), inf)
    | Top.Type(ident, ret_type, ctor_decls) ->
      let let_level = inf.let_level in
      let type_var = make_type_var let_level in
      let ident_case = Ident.intern (sprintf "case%s" ident.Ident.name) in
      let case_type = make_case_type top.Top.pos type_var ret_type ctor_decls in
      let case_scm = generalize let_level case_type in
      let ctor_asp = make_ctor_asp ret_type ctor_decls in
      let inf = {inf with asp=(ident_case,case_scm)::inf.asp} in
      let inf = {inf with asp=List.append ctor_asp inf.asp} in
      (Scheme.mono (Type.Con(top.Top.pos, Ident.intern "()")), inf)
  end

let declare inf decl =
  begin match decl with
    | Decl.Val(ident,scm) ->
      let inf_body = {inf with asp=(ident,scm)::inf.asp} in
      (scm, inf_body)
  end
