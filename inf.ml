type t = {
  dummy : unit;
}

let rec create = begin fun (()(_)) ->
  {
    dummy = ();
  }
end

