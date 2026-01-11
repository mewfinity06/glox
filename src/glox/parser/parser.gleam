// Prefix parse for unary minus
fn parse_unary(chunk: Chunk, tokens: List(Token)) -> ParserResultInner {
  case tokens {
    [Token(loc, lex.Minus), ..tail] ->
      // Parse the right-hand side as a unary expression
      case parse_precedence_prefix(chunk, tail, prec.Unary) {
        Ok(#(chunk, tokens)) -> emit_byte(chunk, op.OpNeg, loc, tokens)
        Error(e) -> Error(e)
      }
    [tok, ..tail] -> pri_error(chunk, Expected(lex.Minus, tok), tail)
    [] -> pri_error(chunk, UnexpectedEof(Loc("parse_unary", 0, 0)), tokens)
  }
}

// Prefix parse for grouping (parentheses)
fn parse_grouping(chunk: Chunk, tokens: List(Token)) -> ParserResultInner {
  case parse_expression(chunk, tokens) {
    Ok(#(chunk, [Token(_, lex.RParen), ..tail])) -> next(chunk, tail)
    Ok(#(chunk, [tok, ..])) ->
      pri_error(chunk, Expected(lex.RParen, tok), tokens)
    Ok(_) ->
      pri_error(chunk, UnexpectedEof(Loc("parse_grouping", 0, 0)), tokens)
    Error(e) -> Error(e)
  }
}

// Helper to parse an expression (used by grouping)
fn parse_expression(chunk: Chunk, tokens: List(Token)) -> ParserResultInner {
  parse_precedence_prefix(chunk, tokens, prec.None)
}

import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}

import gecko/lexer.{type Loc, Loc} as _

import glox/chunk.{type Chunk}
import glox/lexer.{type Token, type TokenType, Token, tt_debug} as lex
import glox/opcode.{type OpCode} as op
import glox/parser/precedence.{type Precedence} as prec

// import glox/parser/precedence.{type Precedence, binding_power as bp}
import glox/values.{type Value}

pub type ParserResult =
  Result(Chunk, #(Chunk, ParserError))

type ParserResultInner =
  Result(#(Chunk, List(Token)), #(Chunk, ParserError, List(Token)))

fn pri_error(
  chunk: Chunk,
  error: ParserError,
  tokens: List(Token),
) -> ParserResultInner {
  Error(#(chunk, error, tokens))
}

fn pri_expose(pri: ParserResultInner) -> ParserResult {
  case pri {
    Ok(#(chunk, _tokens)) -> Ok(chunk)
    Error(#(chunk, err, _tokens)) -> Error(#(chunk, err))
  }
}

/// Parses a list of tokens into a chunk of bytecode.
/// Returns `Ok(Chunk)` on success, or `Error(#(Chunk, ParserError))` on failure.
pub fn parse(chunk: Chunk, tokens: List(Token)) -> ParserResult {
  next(chunk, tokens) |> pri_expose()
}

/// Advances the parser and emits bytecode for the next token(s).
fn next(chunk: Chunk, tokens: List(Token)) -> ParserResultInner {
  case tokens {
    [] -> emit_return(chunk, Loc("", 0, 0), [])
    [Token(loc, lex.Semi), ..tail] -> emit_return(chunk, loc, tail)
    tokens -> parse_expression(chunk, tokens)
  }
}

// PARSING FUNCTIONS
fn parse_precedence_prefix(
  chunk: Chunk,
  tokens: List(Token),
  prec: prec.Precedence,
) -> ParserResultInner {
  case tokens {
    [head, ..tail] -> {
      let ParserRule(prefix, _infix, _prec) = get_rule(head.ty)
      case prefix {
        Some(p) -> {
          // Parse the prefix (e.g., number, unary)
          case p(chunk, tokens) {
            Ok(#(mut_chunk, mut_tokens)) -> {
              // Pratt loop: parse infix as long as precedence increases
              parse_infix_loop(mut_chunk, mut_tokens, prec)
            }
            Error(e) -> Error(e)
          }
        }
        None -> {
          let Token(loc, ty) = head
          pri_error(chunk, UnexpectedToken(loc, ty, "expression"), tail)
        }
      }
    }
    [] ->
      pri_error(
        chunk,
        UnexpectedEof(Loc("parse_precedence_prefix", 0, 0)),
        tokens,
      )
  }
}

fn parse_infix_loop(
  chunk: Chunk,
  tokens: List(Token),
  prec: prec.Precedence,
) -> ParserResultInner {
  case tokens {
    [head, ..] -> {
      let ParserRule(_prefix, infix, infix_prec) = get_rule(head.ty)
      case prec.binding_power(prec) < prec.binding_power(infix_prec) {
        True ->
          case infix {
            Some(p) ->
              case p(chunk, tokens) {
                Ok(#(new_chunk, new_tokens)) ->
                  // Continue looping for more infix ops
                  parse_infix_loop(new_chunk, new_tokens, prec)
                Error(e) -> Error(e)
              }
            None -> Ok(#(chunk, tokens))
          }
        False -> Ok(#(chunk, tokens))
      }
    }
    [] -> Ok(#(chunk, tokens))
  }
}

fn parse_binary(chunk: Chunk, tokens: List(Token)) -> ParserResultInner {
  case tokens {
    [Token(loc, ty), ..tail] -> {
      let op_prec = case ty {
        lex.Plus -> Ok(#(op.OpAdd, prec.Term))
        lex.Minus -> Ok(#(op.OpSub, prec.Term))
        lex.Star -> Ok(#(op.OpMul, prec.Factor))
        lex.Slash -> Ok(#(op.OpDiv, prec.Factor))
        _ ->
          Error(pri_error(chunk, UnexpectedToken(loc, ty, "parse_binary"), tail))
      }
      case op_prec {
        Ok(#(opcode, precedence)) ->
          // Parse the right-hand side with higher precedence
          case
            parse_precedence_prefix(
              chunk,
              tail,
              prec.from_int(prec.binding_power(precedence) + 1),
            )
          {
            Ok(#(chunk, tokens)) -> emit_byte(chunk, opcode, loc, tokens)
            Error(e) -> Error(e)
          }
        Error(e) -> e
      }
    }
    [] -> pri_error(chunk, UnexpectedEof(Loc("parse_binary", 0, 0)), tokens)
  }
}

fn parse_number(chunk: Chunk, tokens: List(Token)) -> ParserResultInner {
  case tokens {
    [Token(loc, lex.Number(x)), ..tail] ->
      emit_const(chunk, int.to_float(x), loc, tail)
    [tok, ..tail] -> pri_error(chunk, Expected(lex.Number(0), tok), tail)
    [] -> pri_error(chunk, UnexpectedEof(Loc("", 0, 0)), tokens)
  }
}

fn parse_float(chunk: Chunk, tokens: List(Token)) -> ParserResultInner {
  case tokens {
    [Token(loc, lex.Float(x)), ..tail] -> emit_const(chunk, x, loc, tail)
    [tok, ..tail] -> pri_error(chunk, Expected(lex.Number(0), tok), tail)
    [] -> pri_error(chunk, UnexpectedEof(Loc("", 0, 0)), tokens)
  }
}

// PARSER RULES
type PrefixParserFn =
  fn(Chunk, List(Token)) -> ParserResultInner

type InfixParserFn =
  fn(Chunk, List(Token)) -> ParserResultInner

type ParserRule {
  ParserRule(
    prefix: Option(PrefixParserFn),
    infix: Option(InfixParserFn),
    precedence: Precedence,
  )
}

fn rule_prefix(prefix: PrefixParserFn, prec: Precedence) -> ParserRule {
  ParserRule(Some(prefix), None, prec)
}

fn rule_infix(infix: InfixParserFn, prec: Precedence) -> ParserRule {
  ParserRule(None, Some(infix), prec)
}

fn rboth(
  prefix: PrefixParserFn,
  infix: InfixParserFn,
  prec: Precedence,
) -> ParserRule {
  ParserRule(Some(prefix), Some(infix), prec)
}

fn get_rule(tt: lex.TokenType) -> ParserRule {
  let rule_opt =
    [
      // Both
      #(fn() { tt == lex.Minus }, rboth(parse_unary, parse_binary, prec.Term)),
      // Prefix
      #(fn() { tt == lex.LParen }, rule_prefix(parse_grouping, prec.None)),
      #(fn() { lex.is_number(tt) }, rule_prefix(parse_number, prec.None)),
      #(fn() { lex.is_float(tt) }, rule_prefix(parse_float, prec.None)),
      // Infix
      #(fn() { tt == lex.Plus }, rule_infix(parse_binary, prec.Term)),
      #(fn() { tt == lex.Slash }, rule_infix(parse_binary, prec.Factor)),
      #(fn() { tt == lex.Star }, rule_infix(parse_binary, prec.Factor)),
      // Base
      #(fn() { tt == lex.RParen }, ParserRule(None, None, prec.None)),
      #(fn() { tt == lex.LBrace }, ParserRule(None, None, prec.None)),
      #(fn() { tt == lex.RBrace }, ParserRule(None, None, prec.None)),
      #(fn() { tt == lex.Comma }, ParserRule(None, None, prec.None)),
      #(fn() { tt == lex.Dot }, ParserRule(None, None, prec.None)),
      #(fn() { tt == lex.Semi }, ParserRule(None, None, prec.None)),
      #(fn() { tt == lex.Bang }, ParserRule(None, None, prec.None)),
      #(fn() { tt == lex.BangEql }, ParserRule(None, None, prec.None)),
      #(fn() { tt == lex.Eql }, ParserRule(None, None, prec.None)),
      #(fn() { tt == lex.EqlEql }, ParserRule(None, None, prec.None)),
      #(fn() { tt == lex.Grtr }, ParserRule(None, None, prec.None)),
      #(fn() { tt == lex.GrtrEql }, ParserRule(None, None, prec.None)),
      #(fn() { tt == lex.Less }, ParserRule(None, None, prec.None)),
      #(fn() { tt == lex.LessEql }, ParserRule(None, None, prec.None)),
      #(fn() { tt == lex.Bang }, ParserRule(None, None, prec.None)),
      #(fn() { tt == lex.KwAnd }, ParserRule(None, None, prec.None)),
      #(fn() { tt == lex.KwClass }, ParserRule(None, None, prec.None)),
      #(fn() { tt == lex.KwElse }, ParserRule(None, None, prec.None)),
      #(fn() { tt == lex.KwFalse }, ParserRule(None, None, prec.None)),
      #(fn() { tt == lex.KwFor }, ParserRule(None, None, prec.None)),
      #(fn() { tt == lex.KwFun }, ParserRule(None, None, prec.None)),
      #(fn() { tt == lex.KwIf }, ParserRule(None, None, prec.None)),
      #(fn() { tt == lex.KwNil }, ParserRule(None, None, prec.None)),
      #(fn() { tt == lex.KwOr }, ParserRule(None, None, prec.None)),
      #(fn() { tt == lex.KwPrint }, ParserRule(None, None, prec.None)),
      #(fn() { tt == lex.KwReturn }, ParserRule(None, None, prec.None)),
      #(fn() { tt == lex.KwSuper }, ParserRule(None, None, prec.None)),
      #(fn() { tt == lex.KwThis }, ParserRule(None, None, prec.None)),
      #(fn() { tt == lex.KwTrue }, ParserRule(None, None, prec.None)),
      #(fn() { tt == lex.KwVar }, ParserRule(None, None, prec.None)),
      #(fn() { tt == lex.KwWhile }, ParserRule(None, None, prec.None)),
      #(fn() { tt == lex.Eof }, ParserRule(None, None, prec.None)),
      #(fn() { lex.is_ident(tt) }, ParserRule(None, None, prec.None)),
      #(fn() { lex.is_string(tt) }, ParserRule(None, None, prec.None)),
    ]
    |> list.find(one_that: fn(i) { i.0() })

  case rule_opt {
    Ok(#(_, rule)) -> rule
    Error(_) -> ParserRule(None, None, prec.None)
  }
}

// EMIT FUNCTIONS
/// Emits a single bytecode instruction into the chunk.
fn emit_byte(
  chunk: Chunk,
  op: OpCode,
  loc: Loc,
  tokens: List(Token),
) -> ParserResultInner {
  let chunk = chunk.write_code(chunk, op, loc.row)
  Ok(#(chunk, tokens))
}

// /// Emits a list of bytecode instructions into the chunk.
// fn emit_bytes(parser: Parser, chunk: Chunk, ops: List(OpCode)) -> Chunk {
//   case ops {
//     [] -> chunk
//     [op, ..ops] -> {
//       let chunk = emit_byte(parser, chunk, op)
//       emit_bytes(parser, chunk, ops)
//     }
//   }
// }

/// Emits a constant value into the chunk.
fn emit_const(
  chunk: Chunk,
  value: Value,
  loc: Loc,
  tokens: List(Token),
) -> ParserResultInner {
  let chunk = chunk.write_const(chunk, value, loc.row)
  Ok(#(chunk, tokens))
}

/// Emits a return instruction into the chunk.
fn emit_return(chunk: Chunk, loc: Loc, tokens: List(Token)) -> ParserResultInner {
  let chunk = chunk.write_code(chunk, op.OpReturn, loc.row)
  Ok(#(chunk, tokens))
}

// PARSER ERROR
pub type ParserError {
  Generic(token: Token, msg: String)
  Expected(expected: TokenType, got: Token)
  ExpectedIdent(loc: Loc, got: TokenType)
  UnexpectedToken(loc: Loc, tt: TokenType, in: String)
  UnexpectedEof(loc: Loc)
  Other(msg: String)
}

/// Returns a string representation of a location in the source file.
fn loc_display(loc: Loc) -> String {
  loc.file <> ":" <> int.to_string(loc.row) <> ":" <> int.to_string(loc.col)
}

/// Converts a `ParserError` to a human-readable string.
pub fn error_print(err: ParserError) -> String {
  case err {
    Generic(Token(loc, tt), msg) ->
      loc_display(loc) <> ": " <> msg <> " @ " <> tt_debug(tt)
    Expected(expected, Token(loc, got)) ->
      loc_display(loc)
      <> ": expected "
      <> tt_debug(got)
      <> " but got "
      <> tt_debug(expected)
    ExpectedIdent(loc, got) ->
      loc_display(loc) <> ": expected ident but got " <> tt_debug(got)
    UnexpectedToken(loc, ty, in) ->
      loc_display(loc) <> ": unexpected token " <> tt_debug(ty) <> " in " <> in
    UnexpectedEof(loc) -> loc_display(loc) <> ": unexpected end of file"
    Other(m) -> m
  }
}
