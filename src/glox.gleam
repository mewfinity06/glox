import gleam/io
import gleam/option.{type Option}
import gleam/string

import argv
import clip.{type Command}
import clip/arg
import clip/help
import clip/opt
import logging as ll
import simplifile as sf

import glox/repl
import glox/runner as run
import glox/vm

// CLI

type Cmd {
  Repl(arg: String)
  File(file: String)
}

/// Returns the CLI command for running the REPL.
fn repl_command() -> Command(Cmd) {
  clip.command({
    use arg <- clip.parameter
    Repl(arg)
  })
  |> clip.opt(opt.new("arg") |> opt.default(""))
  |> clip.help(help.simple("repl", "Run the repl"))
}

/// Returns the CLI command for running a file.
fn file_command() -> Command(Cmd) {
  clip.command({
    use file <- clip.parameter
    File(file)
  })
  |> clip.arg(arg.new("file"))
  |> clip.help(help.simple("file", "Run from a file"))
}

/// Returns the root CLI command for the application.
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
    Ok(Repl(arg)) ->
      case string.length(arg) {
        0 -> repl.run()
        _ -> repl.run_with_command(arg)
      }
    Error(e) -> io.print_error(e <> "\n")
  }
}

// MAIN HELPERS
/// Runs a Glox program from a file and displays the result.
fn from_file(file: String) -> Nil {
  ll.log(ll.Info, "Running file: " <> file)
  case sf.read(file) {
    Ok(source) -> {
      let vm = case run.interpret(file, source) {
        Ok(vm) -> {
          // ll.log(ll.Info, "Successfully ran " <> file)
          vm
        }
        Error(#(vm, _chunk, err)) -> {
          ll.log(ll.Error, err)
          vm
        }
      }
      vm.display(vm)
    }
    Error(_) -> ll.log(ll.Error, "Could not read file " <> file)
  }
}
