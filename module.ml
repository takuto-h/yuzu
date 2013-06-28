type require_argument = bool

type t = {
  mods : ((Names.mod_name * t)) list;
  asp : ((Names.val_name * Scheme.t)) list;
  ctors : ((Names.ctor_name * (require_argument * Scheme.t))) list;
  typectors : ((Names.typector * int)) list;
}

let rec make = begin fun mods ->
  begin fun asp ->
    begin fun ctors ->
      begin fun typectors ->
        {
          mods = mods;
          asp = asp;
          ctors = ctors;
          typectors = typectors;
        }
      end
    end
  end
end

let rec find_asp = begin fun modl ->
  begin fun mod_path ->
    begin fun name ->
      begin match mod_path with
        | ([] _) ->
          ((List.assoc name) modl.asp)
        | (( :: ) (mod_name, mod_path)) ->
          begin let modl = ((List.assoc mod_name) modl.mods) in
          (((find_asp modl) mod_path) name)
          end
      end
    end
  end
end

let rec find_ctor = begin fun modl ->
  begin fun mod_path ->
    begin fun name ->
      begin match mod_path with
        | ([] _) ->
          ((List.assoc name) modl.ctors)
        | (( :: ) (mod_name, mod_path)) ->
          begin let modl = ((List.assoc mod_name) modl.mods) in
          (((find_ctor modl) mod_path) name)
          end
      end
    end
  end
end

