open Printf

type t = {
  gen_num : int;
  preds : (Pred.t) list;
  body : Type.t;
}

let rec mono = begin fun t ->
  {
    gen_num = 0;
    preds = ( [] );
    body = t;
  }
end

let rec poly = begin fun n ->
  begin fun preds ->
    begin fun t ->
      {
        gen_num = n;
        preds = preds;
        body = t;
      }
    end
  end
end

let rec show = begin fun {gen_num;preds;body;} ->
  begin let shower = (Type.create_shower gen_num) in
  (((sprintf "%s %s") ((Pred.show_list shower) preds)) ((Type.show shower) body))
  end
end

