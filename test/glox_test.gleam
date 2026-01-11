import gleam/io
import gleeunit

pub fn main() -> Nil {
  gleeunit.main()
}

// ifth is still kinda said like if
fn ifth(
  cond check: Bool,
  then true_case: fn() -> Nil,
  // elth = else + then
  elth false_case: fn() -> Nil,
) -> Nil {
  case check {
    True -> true_case()
    False -> false_case()
  }
}

pub fn ifth_test() {
  let a = 10
  let b = 1
  ifth(
    cond: a > b,
    then: fn() { io.println("a is greater than b") },
    elth: fn() { io.println("a is smaller than b") },
  )
}
