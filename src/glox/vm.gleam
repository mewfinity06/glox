import gleam/float
import gleam/format.{printf}
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

import glox/chunk.{type Chunk}
import glox/opcode.{type OpCode} as op
import glox/values.{type Value}

import utils/dynarray.{type DynArray} as dyn
import utils/string_helpers as sh

pub type Vm {
  Vm(
    // Code
    chunk: Chunk,
    ip: Int,
    len: Int,
    // Stack
    stack: DynArray(Value),
  )
}

pub fn init(chunk: Chunk) -> Vm {
  let updated_chunk =
    chunk.Chunk(
      codes: chunk.codes,
      lines: chunk.lines,
      consts: chunk.consts,
      start_codes: chunk.codes.items,
      start_lines: chunk.lines.items,
    )
  Vm(
    updated_chunk,
    ip: 0,
    len: list.length(updated_chunk.start_codes),
    stack: dyn.empty(),
  )
}

// VM ERROR

pub type VmError {
  Compile(String)
  Runtime(String)
}

fn expect_error(
  vm: Vm,
  before: OpCode,
  after: OpCode,
  got: OpCode,
) -> Result(Vm, #(Vm, VmError)) {
  Error(#(
    vm,
    Runtime(
      "expected "
      <> op.name(before)
      <> " after "
      <> op.name(after)
      <> ", got "
      <> op.name(got),
    ),
  ))
}

fn error_compile(vm: Vm, msg: String) -> Result(Vm, #(Vm, VmError)) {
  Error(#(vm, Compile(msg)))
}

fn error_runtime(vm: Vm, msg: String) -> Result(Vm, #(Vm, VmError)) {
  Error(#(vm, Runtime(msg)))
}

// VM MANIPULATION

fn next(vm: Vm) -> Option(#(Vm, OpCode)) {
  case vm.ip < vm.len {
    True ->
      case list.drop(vm.chunk.start_codes, vm.ip) {
        [op, ..] -> Some(#(Vm(..vm, ip: vm.ip + 1), op))
        [] -> None
      }
    False -> None
  }
}

// fn peek(vm: Vm) -> Option(OpCode) {
//   case vm.ip < vm.len {
//     True ->
//       case dynarray.get(vm.chunk.codes, vm.ip) {
//         Ok(op) -> Some(op)
//         Error(_) -> None
//       }
//     False -> None
//   }
// }

// STACK MANIPULATION

fn stack_push(vm: Vm, value: Value) -> Result(Vm, VmError) {
  Ok(Vm(..vm, stack: dyn.write_head(vm.stack, value)))
}

fn stack_push_safe(vm: Vm, value: Value) -> Vm {
  case stack_push(vm, value) {
    Ok(ok) -> ok
    Error(_) -> panic as "stack overflow"
  }
}

fn stack_pop(vm: Vm) -> Result(#(Vm, Value), VmError) {
  case vm.stack.items {
    [top, ..rest] -> {
      let new_stack =
        dyn.DynArray(
          count: vm.stack.count - 1,
          capacity: vm.stack.capacity,
          items: rest,
        )
      Ok(#(Vm(..vm, stack: new_stack), top))
    }
    [] -> Error(Runtime("stack underflow"))
  }
}

fn stack_pop_safe(vm: Vm) -> #(Vm, Value) {
  case stack_pop(vm) {
    Ok(ok) -> ok
    Error(_) -> panic as "stack underflow"
  }
}

// fn stack_peek(vm: Vm) -> Result(Value, VmError) {
//   case vm.stack.items {
//     [top, ..] -> Ok(top)
//     [] -> Error(Runtime("stack underflow"))
//   }
// }

// RUN HELPERS
fn binop(vm: Vm, func: fn(Value, Value) -> Value) -> Vm {
  let #(vm, b) = stack_pop_safe(vm)
  let #(vm, a) = stack_pop_safe(vm)
  stack_push_safe(vm, func(a, b))
}

pub fn run(vm: Vm) -> Result(Vm, #(Vm, VmError)) {
  case next(vm) {
    Some(#(vm, op)) ->
      case op {
        op.OpConstant -> {
          case next(vm) {
            Some(#(vm, op.OpValue(index))) -> {
              case dyn.get(vm.chunk.consts, index) {
                Ok(value) -> {
                  let vm = stack_push_safe(vm, value)
                  run(vm)
                }
                Error(_) -> error_runtime(vm, "invalid constant index")
              }
            }
            Some(#(_vm, other)) ->
              expect_error(vm, op.OpValue(0), op.OpConstant, other)
            None -> error_compile(vm, "unexpected end after OpConstant")
          }
        }
        op.OpReturn -> {
          let #(vm, val) = stack_pop_safe(vm)
          io.print("Return: ")
          values.print_value(val)
          Ok(vm)
        }
        op.OpNeg -> {
          let #(vm, val) = stack_pop_safe(vm)
          let vm = stack_push_safe(vm, float.negate(val))
          run(vm)
        }
        op.OpAdd -> {
          let vm = binop(vm, fn(a, b) { a +. b })
          run(vm)
        }
        op.OpSub -> {
          let vm = binop(vm, fn(a, b) { a -. b })
          run(vm)
        }
        op.OpMul -> {
          let vm = binop(vm, fn(a, b) { a *. b })
          run(vm)
        }
        op.OpDiv -> {
          let vm = binop(vm, fn(a, b) { a /. b })
          run(vm)
        }
        unk -> Error(#(vm, Runtime("unhandled opcode: " <> op.display(unk))))
      }
    None -> Ok(vm)
  }
}

fn print_combined_helper(vm: Vm, i: Int, max_len: Int) -> Nil {
  let const_len = vm.chunk.consts.count
  case i >= max_len {
    True -> Nil
    False -> {
      let idx_str = sh.pad_left(int.to_string(i), 4, "0")
      // Print `const` line
      let val_str = case i < const_len {
        False -> ""
        True -> {
          let val = case dyn.get(vm.chunk.consts, i) {
            Ok(v) -> v
            Error(_) -> panic as "consts index out of bounds"
          }
          sh.pad_right(string.inspect(val), 8, " ")
        }
      }
      printf("~s | ~s |", [idx_str, val_str])
      // Print `stack` line
      case dyn.get(vm.stack, i) {
        Ok(val) -> {
          let val_str = sh.pad_right(string.inspect(val), 8, "")
          printf(" ~s | ~s~n", [idx_str, val_str])
          print_combined_helper(vm, i + 1, max_len)
        }
        Error(_) -> printf("~n", [])
      }
    }
  }
}

fn print_combined(vm: Vm) -> Nil {
  printf("-----+----------+------+------~n", [])
  printf("num  | value    | slot | value~n", [])
  printf("-----+----------+------+------~n", [])
  print_combined_helper(vm, 0, int.max(vm.chunk.consts.count, vm.stack.count))
  case vm.chunk.consts.count > vm.stack.count {
    True -> printf("-------+--------|~n", [])
    False -> Nil
  }
}

pub fn display(vm: Vm) -> Nil {
  chunk.disassemble(vm.chunk)
  io.println("")
  print_combined(vm)
}
