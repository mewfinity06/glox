import gleam/format.{printf}

pub type Value =
  Float

/// Prints a value to the standard output.
pub fn print_value(v: Value) -> Nil {
  printf("~g\n", [v])
}
