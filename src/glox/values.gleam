import gleam/format.{printf}

pub type Value =
  Float

pub fn print_value(v: Value) -> Nil {
  printf("~g\n", [v])
}
