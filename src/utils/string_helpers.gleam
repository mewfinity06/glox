import gleam/string

pub fn pad_left(s: String, len: Int, pad: String) -> String {
  let diff = len - string.length(s)
  case diff > 0 {
    True -> string.repeat(pad, diff) <> s
    False -> s
  }
}

pub fn pad_right(s: String, len: Int, pad: String) -> String {
  let diff = len - string.length(s)
  case diff > 0 {
    True -> s <> string.repeat(pad, diff)
    False -> s
  }
}
