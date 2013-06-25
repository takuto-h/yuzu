let rec fold_left = begin fun init ->
  begin fun list ->
    begin fun func ->
      (((List.fold_left func) init) list)
    end
  end
end

let rec fold_right = begin fun list ->
  begin fun init ->
    begin fun func ->
      (((List.fold_right func) list) init)
    end
  end
end

