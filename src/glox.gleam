import argv
import clip.{type Command}
import clip/arg
import clip/help
import gleam/io
import logging as ll

import glox/lexer as lex
import glox/vm

import repl/repl

// CLI

type Cmd {
  Repl
  File(file: String)
}

fn repl_command() -> Command(Cmd) {
  clip.return(Repl)
  |> clip.help(help.simple("repl", "Run the repl"))
}

fn file_command() -> Command(Cmd) {
  clip.command({
    use file <- clip.parameter
    File(file)
  })
  |> clip.arg(arg.new("file"))
  |> clip.help(help.simple("file", "Run from a file"))
}

fn cli() -> Command(Cmd) {
  clip.subcommands_with_default([#("repl", repl_command())], file_command())
}

// MAIN FUNCTION
pub fn main() {
  let result =
    cli()
    |> clip.help(help.simple("glox", "lox interpreter written in Gleam"))
    |> clip.run(argv.load().arguments)

  ll.configure()

  case result {
    Ok(File(file)) -> from_file(file)
    Ok(Repl) -> repl.run()
    Error(e) -> io.print_error(e <> "\n")
  }
}

// MAIN HELPERS
fn from_file(file: String) -> Nil {
  let chunk = lex.init_from_file(file)
  let vm = vm.init(chunk)
  case vm.run(vm) {
    Error(e) -> {
      let vm = case e {
        #(vm, vm.Compile(e)) | #(vm, vm.Runtime(e)) -> {
          ll.log(ll.Error, e)
          vm
        }
      }
      vm.display(vm)
    }
    Ok(final_vm) -> vm.display(final_vm)
  }
}
