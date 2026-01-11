import gleam/int

pub type Precedence {
  None
  Assignment
  Or
  And
  Equality
  Comparison
  Term
  Factor
  Unary
  Call
  Primary
}

pub fn binding_power(p: Precedence) {
  case p {
    None -> 0
    Assignment -> 1
    Or -> 2
    And -> 3
    Equality -> 4
    Comparison -> 5
    Term -> 6
    Factor -> 7
    Unary -> 8
    Call -> 9
    Primary -> 10
  }
}

pub fn from_int(p: Int) {
  case p {
    0 -> None
    1 -> Assignment
    2 -> Or
    3 -> And
    4 -> Equality
    5 -> Comparison
    6 -> Term
    7 -> Factor
    8 -> Unary
    9 -> Call
    10 -> Primary
    _ -> panic as { "invalid precedence: " <> int.to_string(p) }
  }
}
