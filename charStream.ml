
type str_stream = {
  buffer : string;
  mutable curr_pos : int;
}

type t =
  | Str of str_stream

let of_string str =
  Str{buffer=str;curr_pos=0}

let peek strm =
  begin match strm with
    | Str(str_strm) ->
      begin try
        Some(String.get str_strm.buffer str_strm.curr_pos)
      with
        | Invalid_argument(_) -> None
      end
  end

let junk strm =
  begin match strm with
    | Str(str_strm) when str_strm.curr_pos = String.length str_strm.buffer ->
      ()
    | Str(str_strm) ->
      str_strm.curr_pos <- str_strm.curr_pos + 1
  end
