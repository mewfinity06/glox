import gleam/float
import gleam/int
import gleam/list

import gecko as gk
import logging as ll
import simplifile as sf

import glox/chunk.{type Chunk}
import glox/opcode

// TODO: Add Hex values
type Token {
  // Keywords
  Constant
  Return

  Neg
  Add
  Sub
  Mul
  Div

  Number(Int)
  Float(Float)
  Ident(String)

  Eof
}

fn kw_to_op(token: Token) {
  case token {
    Constant -> opcode.OpConstant
    Return -> opcode.OpReturn
    Neg -> opcode.OpNeg
    Add -> opcode.OpAdd
    Sub -> opcode.OpSub
    Mul -> opcode.OpMul
    Div -> opcode.OpDiv
    _ -> panic as "unreachable"
  }
}

fn get_lexer() -> gk.Lexer(Token) {
  gk.Lexer(
    [
      gk.gen_naked("Constant", fn(_) { Constant }),
      gk.gen_naked("Return", fn(_) { Return }),
      gk.gen_naked("Neg", fn(_) { Neg }),
      gk.gen_naked("Add", fn(_) { Add }),
      gk.gen_naked("Sub", fn(_) { Sub }),
      gk.gen_naked("Mul", fn(_) { Mul }),
      gk.gen_naked("Div", fn(_) { Div }),
      gk.gen_rule("[0-9][0-9_]*.[0-9_]*", fn(s) {
        let f = case float.parse(s) {
          Ok(f) -> f
          Error(_) -> {
            ll.log(ll.Error, "Could not parse " <> s)
            0.0
          }
        }
        Float(f)
      }),
      gk.gen_rule("[0-9][0-9_]*", fn(s) {
        let n = case int.parse(s) {
          Ok(n) -> n
          Error(_) -> {
            ll.log(ll.Error, "Could not parse " <> s)
            0
          }
        }
        Number(n)
      }),
      gk.gen_rule("[a-zA-Z][a-zA-Z0-9_]*", Ident),
    ],
    Eof,
  )
}

fn get_tokens(
  lexer: gk.Lexer(Token),
  source: String,
  list: List(Token),
) -> List(Token) {
  case gk.next(lexer, source) {
    #(_, Eof) -> list
    #(s, t) -> get_tokens(lexer, s, list.append(list, [t]))
  }
}

pub fn init_from_file(filepath: String) -> Chunk {
  ll.log(ll.Info, "Reading " <> filepath)
  let lexer = get_lexer()
  let source = case sf.read(filepath) {
    Ok(s) -> s
    Error(_) -> panic as { "could not read file" }
  }
  let tokens = get_tokens(lexer, source, [])
  file_inner(chunk.init(), tokens)
}

fn file_inner(chunk: Chunk, tokens: List(Token)) -> Chunk {
  case tokens {
    [] | [Eof, ..] -> chunk
    [Ident(i), ..] -> panic as { "unknown keywords: " <> i }
    [Number(n), ..xs] ->
      file_inner(chunk.write_const(chunk, int.to_float(n), 0), xs)
    [Float(f), ..xs] -> file_inner(chunk.write_const(chunk, f, 0), xs)
    [x, ..xs] -> file_inner(chunk.write_code(chunk, kw_to_op(x), 0), xs)
  }
}
