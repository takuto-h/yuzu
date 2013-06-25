open Printf

type typevar = ((t) option) ref

and t = {
  pos : (Pos.t) option;
  raw : raw;
}

and raw = 
  | Con of Names.typector
  | Var of (int * typevar)
  | Gen of int
  | App of (Names.typector * (t) list)
  | Tuple of (t) list
  | Fun of (t * t)


let rec at = begin fun pos ->
  begin fun raw ->
    {
      pos = pos;
      raw = raw;
    }
  end
end

let rec make_var = begin fun let_level ->
  ((at None) (Var (let_level, (ref None))))
end

let rec map = begin fun func ->
  begin fun t ->
    begin match t.raw with
      | (Con (_)) ->
        t
      | (Var (_, t_ref)) ->
        begin match (( ! ) t_ref) with
          | (None _) ->
            t
          | (Some (t_val)) ->
            ((map func) t_val)
        end
      | (Gen (n)) ->
        (func n)
      | (App (typector, t_args)) ->
        begin let t_args = ((List.map (map func)) t_args) in
        ((at t.pos) (App (typector, t_args)))
        end
      | (Tuple (ts)) ->
        begin let ts = ((List.map (map func)) ts) in
        ((at t.pos) (Tuple (ts)))
        end
      | (Fun (t_param, t_ret)) ->
        begin let t_param = ((map func) t_param) in
        begin let t_ret = ((map func) t_ret) in
        ((at t.pos) (Fun (t_param, t_ret)))
        end
        end
    end
  end
end

let rec occurs = begin fun t_ref0 ->
  begin fun t ->
    begin match t.raw with
      | (Con (_)) ->
        false
      | (Var (_, t_ref)) ->
        begin match (( ! ) t_ref) with
          | (None _) ->
            ((( == ) t_ref) t_ref0)
          | (Some (t_val)) ->
            ((occurs t_ref0) t_val)
        end
      | (Gen (_)) ->
        (assert false)
      | (App (_, t_args)) ->
        ((List.exists (occurs t_ref0)) t_args)
      | (Tuple (ts)) ->
        ((List.exists (occurs t_ref0)) ts)
      | (Fun (t_param, t_ret)) ->
        (((occurs t_ref0) t_param) || ((occurs t_ref0) t_ret))
    end
  end
end

exception Unification_error of (t * t)

let rec unify = begin fun t1 ->
  begin fun t2 ->
    begin match (t1.raw, t2.raw) with
      | ((Var (_, t1_ref)), (Var (_, t2_ref))) when ((( == ) t1_ref) t2_ref) ->
        ()
      | ((Var (lv1, t1_ref)), (Var (lv2, t2_ref))) ->
        begin match ((( ! ) t1_ref), (( ! ) t2_ref)) with
          | ((Some (t10)), _) ->
            ((unify t10) t2)
          | (_, (Some (t20))) ->
            ((unify t1) t20)
          | ((None _), (None _)) when ((( > ) lv1) lv2) ->
            ((( := ) t1_ref) (Some (t2)))
          | ((None _), (None _)) when ((( < ) lv1) lv2) ->
            ((( := ) t2_ref) (Some (t1)))
          | ((None _), (None _)) ->
            ((( := ) t2_ref) (Some (t1)))
        end
      | ((Var (lv1, t1_ref)), _) ->
        begin match (( ! ) t1_ref) with
          | (Some (t10)) ->
            ((unify t10) t2)
          | (None _) ->
            begin if ((occurs t1_ref) t2) then
              (raise (Unification_error (t1, t2)))
            else
              ((( := ) t1_ref) (Some (t2)))
            end
        end
      | (_, (Var (lv2, t2_ref))) ->
        begin match (( ! ) t2_ref) with
          | (Some (t20)) ->
            ((unify t1) t20)
          | (None _) ->
            begin if ((occurs t2_ref) t1) then
              (raise (Unification_error (t1, t2)))
            else
              ((( := ) t2_ref) (Some (t1)))
            end
        end
      | ((Con (tc1)), (Con (tc2))) when ((( = ) tc1) tc2) ->
        ()
      | ((Con (_)), _) ->
        (raise (Unification_error (t1, t2)))
      | ((Gen (_)), _) ->
        (assert false)
      | ((App (tc1, ts1)), (App (tc2, ts2))) when ((( = ) tc1) tc2) ->
        (((List.iter2 unify) ts1) ts2)
      | ((App (_, _)), _) ->
        (raise (Unification_error (t1, t2)))
      | ((Tuple (ts1)), (Tuple (ts2))) ->
        (((List.iter2 unify) ts1) ts2)
      | ((Tuple (_)), _) ->
        (raise (Unification_error (t1, t2)))
      | ((Fun (t11, t12)), (Fun (t21, t22))) ->
        begin
        ((unify t11) t21);
        ((unify t12) t22)
        end
      | ((Fun (_, _)), _) ->
        (raise (Unification_error (t1, t2)))
    end
  end
end

type shower = {
  mutable var_map : ((typevar * string)) list;
  gen_map : (string) array;
}

let rec create_shower = begin fun gen_num ->
  {
    var_map = [];
    gen_map = ((Array.init gen_num) (sprintf "`t%d"));
  }
end

let rec show = begin fun shower ->
  begin fun t ->
    begin match t.raw with
      | (Con (tc)) ->
        (Names.show_typector tc)
      | (Var (_, t_ref)) ->
        begin match (( ! ) t_ref) with
          | (Some (t_val)) ->
            ((show shower) t_val)
          | (None _) ->
            begin try
              ((List.assq t_ref) shower.var_map)
            with

              | (Not_found _) ->
                begin let str = ((sprintf "`_t%d") (List.length shower.var_map)) in
                begin
                (shower.var_map <- (( :: ) ((t_ref, str), shower.var_map)));
                str
                end
                end
            end
        end
      | (Gen (n)) ->
        ((Array.get shower.gen_map) n)
      | (App (tc, t_args)) ->
        (((sprintf "%s(%s)") (Names.show_typector tc)) (((show_list shower) ", ") t_args))
      | (Tuple (ts)) ->
        ((sprintf "(%s)") (((show_list shower) " * ") ts))
      | (Fun (t_param, t_ret)) ->
        (((sprintf "(%s -> %s)") ((show shower) t_param)) ((show shower) t_ret))
    end
  end
end

and show_list = begin fun shower ->
  begin fun sep ->
    begin fun ts ->
      begin match ts with
        | ([] _) ->
          (assert false)
        | (( :: ) (x, xs)) ->
          (((YzList.fold_left ((show shower) x)) xs) begin fun acc ->
            begin fun elem ->
              ((((sprintf "%s%s%s") acc) sep) ((show shower) elem))
            end
          end)
      end
    end
  end
end

let rec show_origin = begin fun shower ->
  begin fun descr ->
    begin fun t ->
      begin match t.pos with
        | (None _) ->
          ""
        | (Some (pos)) ->
          ((((sprintf "%s: '%s' of %s\n") (Pos.show pos)) ((show shower) t)) descr)
      end
    end
  end
end

