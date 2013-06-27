let rec bind = begin fun option ->
  begin fun func ->
    begin match option with
      | (None _) ->
        None
      | (Some (x)) ->
        (func x)
    end
  end
end

let rec return = begin fun x ->
  (Some (x))
end

