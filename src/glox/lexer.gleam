import gleam/float
import gleam/int
import gleam/list

import gecko/builder as b
import gecko/lexer.{type Loc, Loc}

pub type TokenType {
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
  KwAnd
  KwClass
  KwElse
  KwFalse
  KwFor
  KwFun
  KwIf
  KwNil
  KwOr
  KwPrint
  KwReturn
  KwSuper
  KwThis
  KwTrue
  KwVar
  KwWhile

  Eof
}

/// Returns a configured lexer for the language's token types.
pub fn lexer() -> lexer.Lexer(TokenType) {
  b.init()
  |> b.keywords([
    #("and", KwAnd),
    #("class", KwClass),
    #("else", KwElse),
    #("false", KwFalse),
    #("for", KwFor),
    #("fun", KwFun),
    #("if", KwIf),
    #("nil", KwNil),
    #("or", KwOr),
    #("print", KwPrint),
    #("return", KwReturn),
    #("super", KwSuper),
    #("this", KwThis),
    #("true", KwTrue),
    #("var", KwVar),
    #("while", KwWhile),
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

pub fn is_ident(x) {
  case x {
    Ident(_) -> True
    _ -> False
  }
}

pub fn is_string(x) {
  case x {
    String(_) -> True
    _ -> False
  }
}

pub fn is_float(x) {
  case x {
    Float(_) -> True
    _ -> False
  }
}

pub fn is_number(x) {
  case x {
    Number(_) -> True
    _ -> False
  }
}

// /// Returns a display string for a token type (not implemented).
// pub fn tt_display(tt: TokenType) -> String {
//   case tt {
//     LParen -> todo
//     RParen -> todo
//     LBrace -> todo
//     RBrace -> todo
//     Comma -> todo
//     Dot -> todo
//     Minus -> todo
//     Plus -> todo
//     Semi -> todo
//     Slash -> todo
//     Star -> todo
//     BangEql -> todo
//     Bang -> todo
//     EqlEql -> todo
//     Eql -> todo
//     GrtrEql -> todo
//     Grtr -> todo
//     LessEql -> todo
//     Less -> todo
//     Ident(_) -> todo
//     String(_) -> todo
//     Number(_) -> todo
//     Float(_) -> todo
//     Comment(_) -> todo
//     And -> todo
//     Class -> todo
//     Else -> todo
//     False -> todo
//     For -> todo
//     Fun -> todo
//     If -> todo
//     Nil -> todo
//     Or -> todo
//     Print -> todo
//     Return -> todo
//     Super -> todo
//     This -> todo
//     True -> todo
//     Var -> todo
//     While -> todo
//     Eof -> todo
//   }
// }

/// Returns a debug string for a token type.
pub fn tt_debug(tt: TokenType) -> String {
  case tt {
    LParen -> "LParen"
    RParen -> "RParen"
    LBrace -> "LBrace"
    RBrace -> "RBrace"
    Comma -> "Comma"
    Dot -> "Dot"
    Minus -> "Minus"
    Plus -> "Plus"
    Semi -> "Semi"
    Slash -> "Slash"
    Star -> "Star"
    BangEql -> "BangEql"
    Bang -> "Bang"
    EqlEql -> "EqlEql"
    Eql -> "Eql"
    GrtrEql -> "GrtrEql"
    Grtr -> "Grtr"
    LessEql -> "LessEql"
    Less -> "Less"
    KwAnd -> "And"
    KwClass -> "Class"
    KwElse -> "Else"
    KwFalse -> "False"
    KwFor -> "For"
    KwFun -> "Fun"
    KwIf -> "If"
    KwNil -> "Nil"
    KwOr -> "Or"
    KwPrint -> "Print"
    KwReturn -> "Return"
    KwSuper -> "Super"
    KwThis -> "This"
    KwTrue -> "True"
    KwVar -> "Var"
    KwWhile -> "While"
    Ident(arg) -> "Ident(" <> arg <> ")"
    String(arg) -> "String(" <> arg <> ")"
    Number(arg) -> "Number(" <> int.to_string(arg) <> ")"
    Float(arg) -> "Float(" <> float.to_string(arg) <> ")"
    Comment(_) -> "Comment(" <> "..." <> ")"
    Eof -> "Eof"
  }
}

pub type Token {
  Token(loc: Loc, ty: TokenType)
}

/// Returns an end-of-file token.
pub fn eof() -> Token {
  Token(Loc("", -1, -1), Eof)
}

/// Lexes the source code into a list of tokens.
pub fn lex(
  lexer: lexer.Lexer(TokenType),
  loc: Loc,
  source: String,
  list: List(Token),
) -> List(Token) {
  case lexer.next(lexer, source, loc) {
    #(_, _, Eof) -> list
    #(s, l, t) -> lex(lexer, l, s, list.append(list, [Token(l, t)]))
  }
}
