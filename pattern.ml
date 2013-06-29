open Printf

type t = {
  pos : Pos.t;
  raw : raw;
}

and raw = 
  | Con of Literal.t
  | Var of Names.val_name
  | Ctor of (Names.ctor * (t) option)
  | Tuple of (t) list
  | Record of ((Names.val_path * (t) option)) list
  | Or of (t * t)
  | As of (t * Names.val_name)


let rec at = begin fun pos ->
  begin fun raw ->
    {
      pos = pos;
      raw = raw;
    }
  end
end

