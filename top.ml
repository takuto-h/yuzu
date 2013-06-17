type raw = 
  | Expr of Expr.t
  | LetVal of (Pattern.t * Expr.t)
  | LetFun of ((Names.val_name * Expr.t)) list
  | Open of Names.mod_path
  | Type of ((Names.typector_name * TypeInfo.t)) list
  | Exception of (Names.ctor_name * (TypeExpr.t) option)


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

