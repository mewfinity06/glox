import gleam/int

pub type OpCode {
  OpConstant
  OpReturn
  OpNeg
  OpAdd
  OpSub
  OpMul
  OpDiv
  OpValue(Int)
}

pub fn display(op: OpCode) -> String {
  case op {
    OpConstant -> "OpConstant"
    OpReturn -> "OpReturn"
    OpNeg -> "OpNeg"
    OpAdd -> "OpAdd"
    OpSub -> "OpSub"
    OpMul -> "OpMul"
    OpDiv -> "OpDiv"
    OpValue(v) -> "OpValue(" <> int.to_string(v) <> ")"
  }
}

pub fn name(op: OpCode) -> String {
  case op {
    OpValue(_) -> "OpValue"
    op -> display(op)
  }
}

pub fn operand(op: OpCode) -> String {
  case op {
    OpValue(v) -> int.to_string(v)
    _ -> ""
  }
}
