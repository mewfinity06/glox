import gleam/list

import gecko/builder as b
import gecko/lexer

pub type Token {
  // ( )
  LParen
  RParen
  // { }
  LBrace
  RBrace
  // ,
  Comma
  // .
  Dot
  // -
  Minus
  // +
  Plus
  // ;
  Semi
  // /
  Slash
  // *
  Star
  // != !
  BangEql
  Bang
  // == =
  EqlEql
  Eql
  // >= >
  GrtrEql
  Grtr
  // <= <
  LessEql
  Less
  // Literals
  Ident(String)
  String(String)
  Number(Int)
  Float(Float)
  Comment(String)
  // Keywords
  And
  Class
  Else
  False
  For
  Fun
  If
  Nil
  Or
  Print
  Return
  Super
  This
  True
  Var
  While

  Eof
}

pub fn lexer() -> lexer.Lexer(Token) {
  b.init()
  |> b.keywords([
    #("and", And),
    #("class", Class),
    #("else", Else),
    #("false", False),
    #("for", For),
    #("fun", Fun),
    #("if", If),
    #("nil", Nil),
    #("or", Or),
    #("print", Print),
    #("return", Return),
    #("super", Super),
    #("this", This),
    #("true", True),
    #("var", Var),
    #("while", While),
  ])
  |> b.tokens([
    #("(", LParen),
    #(")", RParen),
    #("{", LBrace),
    #("}", RBrace),
    #(",", Comma),
    #(".", Dot),
    #("-", Minus),
    #("+", Plus),
    #(";", Semi),
    #("/", Slash),
    #("*", Star),
    #("!=", BangEql),
    #("!", Bang),
    #("==", EqlEql),
    #("=", Eql),
    #(">=", GrtrEql),
    #(">", Grtr),
    #("<=", LessEql),
    #("<", Less),
  ])
  |> b.float("[0-9_]*.[0-9_]+", Float)
  |> b.number("[0-9][0-9_]*", Number)
  |> b.ident("[a-zA-Z][a-zA-Z0-9_]*", Ident)
  |> b.comment("//.*", Comment)
  |> b.compile(Eof)
}

pub fn lex(
  lexer: lexer.Lexer(Token),
  source: String,
  list: List(Token),
) -> List(Token) {
  case lexer.next(lexer, source) {
    #(_, Eof) -> list
    #(s, t) -> lex(lexer, s, list.append(list, [t]))
  }
}
