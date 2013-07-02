type raw = 
  | Expr of Expr.t
  | LetVal of (Pattern.t * Expr.t)
  | LetFun of ((Names.val_name * Expr.t * ((Names.val_path) list) ref)) list
  | Open of Names.mod_path
  | Type of (TypeDef.t) list
  | Exception of ExnDecl.t
  | Class of (Names.typeclass_name * TypeExpr.t * ((Names.val_name * SchemeExpr.t)) list)


type t = {
  pos : Pos.t;
  raw : raw;
}

let rec at = begin fun pos ->
  begin fun raw ->
    {
      pos = pos;
      raw = raw;
    }
  end
end

