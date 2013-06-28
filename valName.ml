type t = Names.val_name

let rec compare = begin fun name1 ->
  begin fun name2 ->
    ((String.compare (Names.show_val_name name1)) (Names.show_val_name name2))
  end
end

