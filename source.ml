
type t = {
  mutable line_num : int;
  mutable offset : int;
  stream : char Stream.t;
}

let of_string str = {
  line_num = 1;
  offset = 0;
  stream = Stream.of_string str;
}

let pos src =
  Pos.make src.line_num src.offset

let peek src =
  Stream.peek src.stream

let junk src =
  begin match peek src with
    | None ->
      ()
    | Some('\n') -> begin
      src.line_num <- src.line_num + 1;
      src.offset <- 0;
      Stream.junk src.stream
    end
    | Some('\t') -> begin
      src.offset <- src.offset + 8;
      Stream.junk src.stream
    end
    | Some(_) -> begin
      src.offset <- src.offset + 1;
      Stream.junk src.stream
    end
  end
