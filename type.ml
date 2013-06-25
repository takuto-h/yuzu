type t = 
  | Con of Names.typector
  | Var of (int * ((t) option) ref)
  | Gen of int
  | App of (Names.typector * (t) list)
  | Tuple of (t) list
  | Fun of (t * t)


let rec make_var = begin fun let_level ->
  (Var (let_level, (ref None)))
end

let rec map = begin fun t ->
  begin fun func ->
    begin match t with
      | (Con(_)) ->
        t
      | (Var(_, t_ref)) ->
        begin match (( ! ) t_ref) with
          | (None(_)) ->
            t
          | (Some(t_val)) ->
            ((map t_val) func)
        end
      | (Gen(n)) ->
        (func n)
      | (App(typector, t_args)) ->
        begin let t_args = ((map_list t_args) func) in
        (App (typector, t_args))
        end
      | (Tuple(ts)) ->
        begin let ts = ((map_list ts) func) in
        (Tuple (ts))
        end
      | (Fun(t_param, t_ret)) ->
        begin let t_param = ((map t_param) func) in
        begin let t_ret = ((map t_ret) func) in
        (Fun (t_param, t_ret))
        end
        end
    end
  end
end

and map_list = begin fun ts ->
  begin fun func ->
    begin let ts_ref = (ref []) in
    begin
    ((YzList.iter ts) begin fun t_elem ->
      ((( := ) ts_ref) (( :: ) (((map t_elem) func), (( ! ) ts_ref))))
    end);
    (List.rev (( ! ) ts_ref))
    end
    end
  end
end

let rec occurs = begin fun t_ref0 ->
  begin fun t ->
    begin match t with
      | (Con(_)) ->
        false
      | (Var(_, t_ref)) ->
        begin match (( ! ) t_ref) with
          | (None(_)) ->
            ((( == ) t_ref) t_ref0)
          | (Some(t_val)) ->
            ((occurs t_ref0) t_val)
        end
      | (Gen(_)) ->
        (assert false)
      | (App(_, t_args)) ->
        ((List.exists (occurs t_ref0)) t_args)
      | (Tuple(ts)) ->
        ((List.exists (occurs t_ref0)) ts)
      | (Fun(t_param, t_ret)) ->
        (((occurs t_ref0) t_param) || ((occurs t_ref0) t_ret))
    end
  end
end

let rec unify = begin fun t1 ->
  begin fun t2 ->
    begin fun occurs_check_error ->
      begin fun unification_error ->
        begin match (t1, t2) with
          | ((Var(_, t1_ref)), (Var(_, t2_ref))) when ((( == ) t1_ref) t2_ref) ->
            ()
          | ((Var(lv1, t1_ref)), (Var(lv2, t2_ref))) ->
            begin match ((( ! ) t1_ref), (( ! ) t2_ref)) with
              | ((Some(t10)), _) ->
                ((((unify t10) t2) occurs_check_error) unification_error)
              | (_, (Some(t20))) ->
                ((((unify t1) t20) occurs_check_error) unification_error)
              | ((None(_)), (None(_))) when ((( > ) lv1) lv2) ->
                ((( := ) t1_ref) (Some (t2)))
              | ((None(_)), (None(_))) when ((( < ) lv1) lv2) ->
                ((( := ) t2_ref) (Some (t1)))
              | ((None(_)), (None(_))) ->
                ((( := ) t2_ref) (Some (t1)))
            end
          | ((Var(lv1, t1_ref)), _) ->
            begin match (( ! ) t1_ref) with
              | (Some(t10)) ->
                ((((unify t10) t2) occurs_check_error) unification_error)
              | (None(_)) ->
                begin if ((occurs t1_ref) t2) then
                  (occurs_check_error ())
                else
                  ((( := ) t1_ref) (Some (t2)))
                end
            end
          | (_, (Var(lv2, t2_ref))) ->
            begin match (( ! ) t2_ref) with
              | (Some(t20)) ->
                ((((unify t1) t20) occurs_check_error) unification_error)
              | (None(_)) ->
                begin if ((occurs t2_ref) t1) then
                  (occurs_check_error ())
                else
                  ((( := ) t2_ref) (Some (t1)))
                end
            end
          | ((Con(tc1)), (Con(tc2))) when ((( = ) tc1) tc2) ->
            ()
          | ((Con(_)), _) ->
            ((unification_error t1) t2)
          | ((Gen(_)), _) ->
            (assert false)
          | ((App(tc1, ts1)), (App(tc2, ts2))) when ((( = ) tc1) tc2) ->
            ((((unify_list ts1) ts2) occurs_check_error) unification_error)
          | ((App(_, _)), _) ->
            ((unification_error t1) t2)
          | ((Tuple(ts1)), (Tuple(ts2))) ->
            ((((unify_list ts1) ts2) occurs_check_error) unification_error)
          | ((Tuple(_)), _) ->
            ((unification_error t1) t2)
          | ((Fun(t11, t12)), (Fun(t21, t22))) ->
            begin
            ((((unify t11) t21) occurs_check_error) unification_error);
            ((((unify t12) t22) occurs_check_error) unification_error)
            end
          | ((Fun(_, _)), _) ->
            ((unification_error t1) t2)
        end
      end
    end
  end
end

and unify_list = begin fun ts1 ->
  begin fun ts2 ->
    begin fun occurs_check_error ->
      begin fun unification_error ->
        (((YzList.iter2 ts1) ts2) begin fun t1 ->
          begin fun t2 ->
            ((((unify t1) t2) occurs_check_error) unification_error)
          end
        end)
      end
    end
  end
end

