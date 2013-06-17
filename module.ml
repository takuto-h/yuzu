type t = {
  mods : ((Names.mod_name * t)) list;
  asp : ((Names.val_name * Type.t)) list;
}

let rec make = begin fun mods ->
  begin fun asp ->
    {
      mods = mods;
      asp = asp;
    }
  end
end

let rec find_asp = begin fun modl ->
  begin fun mod_path ->
    begin fun name ->
      begin match mod_path with
        | ([](_)) ->
          ((List.assoc name) modl.asp)
        | (( :: )(mod_name, mod_path)) ->
          begin let modl = ((List.assoc mod_name) modl.mods) in
          (((find_asp modl) mod_path) name)
          end
      end
    end
  end
end

