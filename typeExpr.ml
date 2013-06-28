type t = {
  pos : Pos.t;
  raw : raw;
}

and raw = 
  | Con of Names.typector
  | Var of string
  | App of (Names.typector * (t) list)
  | Tuple of (t) list
  | Fun of (t * t)


let rec at = begin fun pos ->
  begin fun raw ->
    {
      pos = pos;
      raw = raw;
    }
  end
end

