import gleam/int

pub type OpCode {
  OpConstant
  OpReturn
  OpNeg
  OpNot
  OpDiv
  OpNil
  OpTrue
  OpEql
  OpGrtr
  OpLess
  OpAdd
  OpSub
  OpMul
  OpFalse
  OpValue(sp: Int)
}

/// Returns a string representation of an opcode.
pub fn display(op: OpCode) -> String {
  case op {
    OpConstant -> "OpConstant"
    OpReturn -> "OpReturn"
    OpNeg -> "OpNeg"
    OpNot -> "OpNot"
    OpEql -> "OpEql"
    OpGrtr -> "OpGrtr"
    OpLess -> "OpLess"
    OpAdd -> "OpAdd"
    OpSub -> "OpSub"
    OpMul -> "OpMul"
    OpDiv -> "OpDiv"
    OpNil -> "OpNil"
    OpTrue -> "OpTrue"
    OpFalse -> "OpFalse"
    OpValue(v) -> "OpValue(" <> int.to_string(v) <> ")"
  }
}

/// Returns the name of an opcode.
pub fn name(op: OpCode) -> String {
  case op {
    OpValue(_) -> "OpValue"
    op -> display(op)
  }
}

/// Returns the operand of an opcode as a string, if any.
pub fn operand(op: OpCode) -> String {
  case op {
    OpValue(v) -> int.to_string(v)
    _ -> ""
  }
}
