type t = 
  | Con of Names.typector
  | Var of (int * ((t) option) ref)
  | Gen of int
  | App of (Names.typector * (t) list)
  | Tuple of (t) list


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
    end
  end
end

and map_list = begin fun ts ->
  begin fun func ->
    begin let ts_ref = (ref []) in
    begin
    ((YzList.iteri ts) begin fun i ->
      begin fun t_elem ->
        ((( := ) ts_ref) (( :: ) (((map t_elem) func), (( ! ) ts_ref))))
      end
    end);
    (List.rev (( ! ) ts_ref))
    end
    end
  end
end

