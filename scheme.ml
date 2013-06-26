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

let rec show = begin fun {gen_num;body;} ->
  begin let shower = (Type.create_shower gen_num) in
  ((Type.show shower) body)
  end
end

