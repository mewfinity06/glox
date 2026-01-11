import gleam/format.{printf}
import gleam/int
import gleam/list
import gleam/string

import glox/opcode.{type OpCode, OpValue}
import glox/values.{type Value}

import utils/dynarray.{type DynArray} as dyn
import utils/string_helpers as sh

pub type Chunk {
  Chunk(
    codes: DynArray(OpCode),
    lines: DynArray(Int),
    consts: DynArray(Value),
    start_codes: List(OpCode),
    start_lines: List(Int),
  )
}

pub fn init() -> Chunk {
  Chunk(
    codes: dyn.empty(),
    lines: dyn.empty(),
    consts: dyn.empty(),
    start_codes: [],
    start_lines: [],
  )
}

// WRITE FUNCTIONS //

pub fn write_code(chunk: Chunk, code: OpCode, line: Int) -> Chunk {
  Chunk(
    codes: dyn.write(chunk.codes, code),
    lines: dyn.write(chunk.lines, line),
    consts: chunk.consts,
    start_codes: chunk.start_codes,
    start_lines: chunk.start_lines,
  )
}

pub fn write_const(chunk: Chunk, value: Value, line: Int) -> Chunk {
  Chunk(
    codes: dyn.write(chunk.codes, OpValue(chunk.consts.count)),
    lines: dyn.write(chunk.lines, line),
    consts: dyn.write(chunk.consts, value),
    start_codes: chunk.start_codes,
    start_lines: chunk.start_lines,
  )
}

pub fn write_from_list(chunk: Chunk, list: List(OpCode)) -> Chunk {
  list.fold(list, chunk, fn(acc, code) { write_code(acc, code, 0) })
}

// DISASSEMBLY FUNCTIONS //

pub fn disassemble(chunk: Chunk) -> Nil {
  printf("-----+------+------------+--------~n", [])
  printf("addr | line | opcode     | operand~n", [])
  printf("-----+------+------------+--------~n", [])
  disassemble_insts(chunk, chunk.start_codes, chunk.start_lines, 0, -1)
  printf("-----+------+------------+--------~n", [])
}

fn disassemble_insts(
  chunk: Chunk,
  op: List(OpCode),
  lines: List(Int),
  offset: Int,
  prev_line: Int,
) -> Nil {
  case op, lines {
    [x, ..xs], [ln, ..ls] -> {
      let line_str = case ln == prev_line {
        True -> sh.pad_left("-", 4, " ")
        False -> sh.pad_left(int.to_string(ln), 4, " ")
      }
      let op_name = opcode.name(x)
      let operand = case x {
        opcode.OpValue(index) -> {
          let value = case dyn.get(chunk.consts, index) {
            Ok(v) -> v
            Error(_) -> panic as "const index out of bounds"
          }
          string.inspect(value) <> " "
        }
        _ -> ""
      }

      // <addr> | <line> | <opcode> | <operand>
      let addr_str = sh.pad_left(int.to_string(offset), 4, "0")
      let op_str = sh.pad_right(op_name, 10, " ")
      let operand_str = operand
      printf("~s | ~s | ~s | ~s~n", [addr_str, line_str, op_str, operand_str])
      disassemble_insts(chunk, xs, ls, offset + 1, ln)
    }
    _, _ -> Nil
  }
}
