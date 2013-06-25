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

let rec iteri = begin fun list ->
  begin fun func ->
    begin let rec loop = begin fun i ->
      begin fun list ->
        begin match list with
          | ([](_)) ->
            ()
          | (( :: )(x, xs)) ->
            begin
            ((func i) x);
            ((loop ((( + ) i) 1)) xs)
            end
        end
      end
    end in
    ((loop 0) list)
    end
  end
end

