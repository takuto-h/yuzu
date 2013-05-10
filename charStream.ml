
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

let peek strm =
  Stream.peek strm.stream

let junk strm =
  begin match peek strm with
    | None ->
      ()
    | Some('\n') -> begin
      strm.line_num <- strm.line_num + 1;
      strm.offset <- 0;
      Stream.junk strm.stream
    end
    | Some('\t') -> begin
      strm.offset <- strm.offset + 8;
      Stream.junk strm.stream
    end
    | Some(_) -> begin
      strm.offset <- strm.offset + 1;
      Stream.junk strm.stream
    end
  end
