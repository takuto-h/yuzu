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

let rec iter = begin fun list ->
  begin fun func ->
    ((List.iter func) list)
  end
end

let rec iter2 = begin fun list1 ->
  begin fun list2 ->
    begin fun func ->
      (((List.iter2 func) list1) list2)
    end
  end
end

