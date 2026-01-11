import glox/chunk.{type Chunk}
import glox/lexer
import glox/parser/parser
import glox/vm.{type Vm}

import gecko/lexer.{Loc} as _

/// Interprets the given source code from a file path, compiling and running it.
/// Returns `Ok(Vm)` on success, or `Error(#(Vm, Chunk, String))` on failure.
pub fn interpret(
  file_path: String,
  source: String,
) -> Result(Vm, #(Vm, Chunk, String)) {
  let chunk = chunk.init()

  case compile(chunk, file_path, source) {
    Ok(chunk) -> {
      let vm = vm.init(chunk)
      case vm.run(vm) {
        Ok(vm) -> Ok(vm)
        Error(#(vm, e)) -> Error(#(vm, chunk, vm.error_to_string(e)))
      }
    }
    Error(#(chunk, e)) -> Error(#(vm.empty(), chunk, e))
  }
}

/// Compiles the given source code into a chunk of bytecode.
/// Returns `Ok(Chunk)` on success, or `Error(#(Chunk, String))` on failure.
pub fn compile(
  chunk: Chunk,
  file_path: String,
  source: String,
) -> Result(Chunk, #(Chunk, String)) {
  let lexer = lexer.lexer()
  let tokens = lexer.lex(lexer, Loc(file_path, 0, 0), source, [])
  case parser.parse(chunk, tokens) {
    Ok(c) -> Ok(c)
    Error(#(chunk, e)) -> Error(#(chunk, parser.error_print(e)))
  }
}
