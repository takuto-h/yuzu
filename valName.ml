
type t = {
  value : string;
}

let make str = {
  value = str;
}

let show {value=str} =
  str
