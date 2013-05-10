
type source =
  | Str of string

type t = {
  inner_stream : char Stream.t;
  source : source;
}

let of_string str = {
  inner_stream = Stream.of_string str;
  source = Str(str);
}

let peek strm =
  Stream.peek strm.inner_stream

let junk strm =
  Stream.junk strm.inner_stream

let show_line _ _ =
  ""
