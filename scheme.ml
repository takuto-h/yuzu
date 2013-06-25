type t = {
  gen_num : int;
  body : Type.t;
}

let rec mono = begin fun t ->
  {
    gen_num = 0;
    body = t;
  }
end

