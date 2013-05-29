
type t = {
  name : string;
}

let intern str = {
  name = str;
}

let show {name} =
  name
