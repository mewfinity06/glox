import gleam/io
import gleam/string

import gecko/lexer.{Loc} as _
import in
import logging as ll

// import glox/runner as run
import glox/chunk.{type Chunk}
import glox/lexer
import glox/parser/parser
import glox/runner as run
import glox/vm.{type Vm}

/// Runs the Glox REPL loop.
pub fn run() -> Nil {
  // ll.log(ll.Info, "Running Glox repl")
  case loop() {
    Ok(_) -> Nil
    Error(e) -> ll.log(ll.Error, e.2)
  }
}

pub fn run_with_command(s: String) -> Nil {
  case run.interpret("repl", s) {
    Ok(_) -> Nil
    Error(e) -> {
      io.println_error("Error: " <> e.2)
      Nil
    }
  }
}

/// The main REPL loop, reading and evaluating user input.
fn loop() -> Result(Nil, #(Vm, Chunk, String)) {
  io.print("> ")
  case in.read_line() {
    Ok(line) ->
      case string.trim(line) {
        "" -> loop()
        "quit;" -> Ok(Nil)
        line -> {
          case run.interpret("repl", line) {
            Ok(_) -> loop()
            Error(e) -> {
              io.println_error("Error: " <> e.2)
              loop()
            }
          }
        }
      }
    Error(_) -> Error(#(vm.empty(), chunk.init(), "Could not read line"))
  }
}

fn get_chunk(s: String) {
  let lexer = lexer.lexer()
  let tokens = lexer.lex(lexer, Loc("repl", 0, 0), s, [])
  parser.parse(chunk.init(), tokens)
}
