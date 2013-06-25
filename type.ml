type t = 
  | Con of Names.typector
  | Var of (int * ((t) option) ref)
  | Gen of int
  | App of (Names.typector * (t) list)
  | Tuple of (t) list


let rec map = begin fun func ->
  begin fun t ->
    begin match t with
      | (Con(_)) ->
        t
      | (Var(_, t_ref)) ->
        begin match (( ! ) t_ref) with
          | (None(_)) ->
            t
          | (Some(t_val)) ->
            ((map func) t_val)
        end
      | (Gen(n)) ->
        (func n)
      | (App(typector, t_args)) ->
        begin let t_args = ((map_list func) t_args) in
        (App (typector, t_args))
        end
      | (Tuple(ts)) ->
        begin let ts = ((map_list func) ts) in
        (Tuple (ts))
        end
    end
  end
end

and map_list = begin fun func ->
  begin fun ts ->
    begin let ts_ary = ((Array.make 0) (Gen (0))) in
    begin
    ((YzList.iteri ts) begin fun i ->
      begin fun t_elem ->
        (((Array.set ts_ary) i) ((map func) t_elem))
      end
    end);
    (Array.to_list ts_ary)
    end
    end
  end
end

