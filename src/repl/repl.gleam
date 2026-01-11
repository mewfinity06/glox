import gleam/io

import in
import logging as ll

import glox/chunk
import glox/runner as run
import glox/vm.{type Vm}

pub fn run() -> Nil {
  ll.log(ll.Info, "Running Glox repl")
  let vm = vm.init(chunk.init())
  case loop(vm) {
    Ok(_) -> ll.log(ll.Info, "Repl exited successfully")
    Error(e) -> ll.log(ll.Error, e)
  }
}

fn loop(vm: Vm) -> Result(Nil, String) {
  io.print("> ")
  case in.read_line() {
    Ok(line) ->
      case line {
        ":quit" -> Ok(Nil)
        _ ->
          case run.interpret(vm, line) {
            Ok(vm) -> loop(vm)
            Error(e) -> Error(e)
          }
      }
    Error(_) -> Error("Could not read line")
  }
}
