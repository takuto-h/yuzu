let rec bind = begin fun opt ->
  begin fun func ->
    begin match opt with
      | None ->
        None
      | (Some (x)) ->
        (func x)
    end
  end
end

let rec return = begin fun x ->
  (Some x)
end

let rec or_ = begin fun opt1 ->
  begin fun opt2 ->
    begin match opt1 with
      | (Some (x)) ->
        opt1
      | None ->
        opt2
    end
  end
end

