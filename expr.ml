type t = {
  pos : Pos.t;
  raw : raw;
}

and raw = 
  | Con of Literal.t
  | Var of (Names.val_path * (((Names.mod_path) ref) list) ref)
  | Abs of (Pattern.t * t)
  | App of (t * t)
  | Ctor of (Names.ctor * (t) option)
  | If of (t * t * t)
  | Tuple of (t) list
  | Or of (t * t)
  | And of (t * t)
  | Seq of (t * t)
  | LetVal of (Pattern.t * t * t)
  | LetFun of (((Names.val_name * t * ((Names.mod_path) list) ref)) list * t)
  | Match of (t * ((Pattern.t * (t) option * t)) list)
  | Try of (t * ((Pattern.t * (t) option * t)) list)
  | Record of ((Names.val_path * t)) list
  | Update of (t * ((Names.val_path * t)) list)
  | Field of (t * Names.val_path)
  | Assign of (t * Names.val_path * t)


let rec at = begin fun pos ->
  begin fun raw ->
    {
      pos = pos;
      raw = raw;
    }
  end
end

