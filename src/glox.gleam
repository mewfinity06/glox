import argv
import clip.{type Command}
import clip/arg
import clip/help
import gleam/io
import logging as ll

import glox/checker/lexer as lex
import glox/repl/runner as repl

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
fn from_file(_file: String) -> Nil {
  ll.log(ll.Info, "Tokens:")
  let lexer = lex.lexer()
  let tokens = lex.lex(lexer, "print 1 + 2;", [])
  echo tokens
  Nil
}
