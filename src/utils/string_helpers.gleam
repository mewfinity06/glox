import gleam/string

/// Pads the string on the left to the given length with the pad string.
pub fn pad_left(s: String, len: Int, pad: String) -> String {
  let diff = len - string.length(s)
  case diff > 0 {
    True -> string.repeat(pad, diff) <> s
    False -> s
  }
}

/// Pads the string on the right to the given length with the pad string.
pub fn pad_right(s: String, len: Int, pad: String) -> String {
  let diff = len - string.length(s)
  case diff > 0 {
    True -> s <> string.repeat(pad, diff)
    False -> s
  }
}
