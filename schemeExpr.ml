type t = {
  preds : (PredExpr.t) list;
  body : TypeExpr.t;
}

let rec make = begin fun preds ->
  begin fun body ->
    {
      preds = preds;
      body = body;
    }
  end
end

