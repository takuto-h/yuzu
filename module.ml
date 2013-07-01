type require_argument = bool

type mutability = bool

type t = {
  mods : ((Names.mod_name * t)) list;
  asp : ((Names.val_name * Scheme.t)) list;
  ctors : ((Names.ctor_name * (require_argument * Scheme.t))) list;
  fields : ((Names.ctor_name * (mutability * Scheme.t))) list;
  typectors : ((Names.typector_name * (Names.typector * int * (Scheme.t) option))) list;
  typeclasses : ((Names.typeclass_name * ((Names.typector * unit)) list)) list;
}

let rec make = begin fun mods ->
  begin fun asp ->
    begin fun ctors ->
      begin fun fields ->
        begin fun typectors ->
          begin fun typeclasses ->
            {
              mods = mods;
              asp = asp;
              ctors = ctors;
              fields = fields;
              typectors = typectors;
              typeclasses = typeclasses;
            }
          end
        end
      end
    end
  end
end

let rec search_alist = begin fun get_alist ->
  begin fun modl ->
    begin fun mod_path ->
      begin fun name ->
        begin match mod_path with
          | ( [] ) ->
            ((List.assoc name) (get_alist modl))
          | (( :: ) (mod_name, mod_path)) ->
            begin let modl = ((List.assoc mod_name) modl.mods) in
            ((((search_alist get_alist) modl) mod_path) name)
            end
        end
      end
    end
  end
end

let rec search_asp = begin fun modl ->
  begin fun mod_path ->
    begin fun name ->
      ((((search_alist begin fun modl ->
        modl.asp
      end) modl) mod_path) name)
    end
  end
end

let rec search_ctors = begin fun modl ->
  begin fun mod_path ->
    begin fun name ->
      ((((search_alist begin fun modl ->
        modl.ctors
      end) modl) mod_path) name)
    end
  end
end

let rec search_fields = begin fun modl ->
  begin fun mod_path ->
    begin fun name ->
      ((((search_alist begin fun modl ->
        modl.fields
      end) modl) mod_path) name)
    end
  end
end

let rec search_typectors = begin fun modl ->
  begin fun mod_path ->
    begin fun name ->
      ((((search_alist begin fun modl ->
        modl.typectors
      end) modl) mod_path) name)
    end
  end
end

let rec search_typeclasses = begin fun modl ->
  begin fun mod_path ->
    begin fun name ->
      ((((search_alist begin fun modl ->
        modl.typeclasses
      end) modl) mod_path) name)
    end
  end
end

