open Printf

type t = 
  | IsIn of (Names.typeclass * Type.t)


let rec show = begin fun shower ->
  begin fun pred ->
    begin match pred with
      | (IsIn (tc, t)) ->
        (((sprintf "%s(%s)") (Names.show_typeclass tc)) ((Type.show shower) t))
    end
  end
end

let rec show_list = begin fun shower ->
  begin fun preds ->
    begin match preds with
      | ( [] ) ->
        ""
      | (( :: ) (pred, preds)) ->
        begin let str_list = (((YzList.fold_left ((show shower) pred)) preds) begin fun acc ->
          begin fun pred ->
            (((sprintf "%s, %s") acc) ((show shower) pred))
          end
        end) in
        ((sprintf "(%s) =>") str_list)
        end
    end
  end
end

