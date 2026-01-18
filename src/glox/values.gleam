import gleam/float
import gleam/int

pub type Value {
  ValBool(Bool)
  ValNumber(Int)
  ValFloat(Float)
  ValNil
}

pub fn is_bool(v) {
  case v {
    ValBool(_) -> True
    _ -> False
  }
}

pub fn is_number(v) {
  case v {
    ValNumber(_) -> True
    _ -> False
  }
}

pub fn is_float(v) {
  case v {
    ValFloat(_) -> True
    _ -> False
  }
}

pub fn is_nil(v) {
  case v {
    ValNil -> True
    _ -> False
  }
}

pub fn is_same(v1, v2) {
  case v1, v2 {
    ValBool(_), ValBool(_) -> True
    ValNumber(_), ValNumber(_) -> True
    ValFloat(_), ValFloat(_) -> True
    ValNil, ValNil -> True
    _, _ -> False
  }
}

pub fn equal(v1, v2) {
  case v1, v2 {
    ValBool(a), ValBool(b) -> a == b
    ValNumber(a), ValNumber(b) -> a == b
    ValFloat(a), ValFloat(b) -> a == b
    ValNil, ValNil -> True
    _, _ -> False
  }
}

/// Prints a value to the standard output.
pub fn display(v: Value) -> String {
  case v {
    ValBool(b) ->
      case b {
        True -> "true"
        False -> "false"
      }
    ValNumber(x) -> int.to_string(x)
    ValFloat(x) -> float.to_string(x)
    ValNil -> "nil"
  }
}

pub fn debug(v: Value) -> String {
  case v {
    ValBool(b) ->
      "ValBool("
      <> case b {
        True -> "true"
        False -> "false"
      }
      <> ")"
    ValNumber(x) -> "ValNumber(" <> int.to_string(x) <> ")"
    ValFloat(x) -> "ValFloat(" <> float.to_string(x) <> ")"
    ValNil -> "ValNil"
  }
}

pub fn name(v: Value) -> String {
  case v {
    ValBool(_) -> "ValBool"
    ValNumber(_) -> "ValNumber"
    ValFloat(_) -> "ValFloat"
    ValNil -> "ValNil"
  }
}
