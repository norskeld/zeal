use crate::span::Span;

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum TokenKind {
  Add,
  AddAssign,
  And,
  Arrow,
  As,
  Assign,
  Async,
  Await,
  Bang,
  BitAnd,
  BitAndAssign,
  BitOr,
  BitOrAssign,
  BitXor,
  BitXorAssign,
  BracketClose,
  BracketOpen,
  Break,
  Builtin,
  Case,
  Colon,
  Comma,
  Comment,
  Const,
  BraceClose,
  BraceOpen,
  Div,
  DivAssign,
  Dot,
  DoubleArrow,
  DoubleColon,
  DoubleStringClose,
  DoubleStringOpen,
  Else,
  Enum,
  Eq,
  Extern,
  False,
  Float,
  Fn,
  For,
  Ge,
  Gt,
  Identifier,
  If,
  Impl,
  Integer,
  Invalid,
  InvalidEscape,
  Iso,
  Le,
  Let,
  Loop,
  Lt,
  Match,
  Mod,
  ModAssign,
  Move,
  Mul,
  MulAssign,
  Mut,
  Ne,
  Next,
  Null,
  Or,
  ParenClose,
  ParenOpen,
  Pow,
  PowAssign,
  Pub,
  Recover,
  Ref,
  Replace,
  Return,
  SelfType,
  SelfValue,
  Shl,
  ShlAssign,
  Shr,
  ShrAssign,
  SingleStringClose,
  SingleStringOpen,
  StringExprClose,
  StringExprOpen,
  StringText,
  Struct,
  Sub,
  SubAssign,
  Trait,
  True,
  Try,
  TryOp,
  UnicodeEscape,
  UnsignedShr,
  UnsignedShrAssign,
  Use,
  While,
  Whitespace,
}

impl TokenKind {
  pub fn description(&self) -> &str {
    match self {
      | TokenKind::Add => "a '+'",
      | TokenKind::AddAssign => "a '+='",
      | TokenKind::And => "an '&&'",
      | TokenKind::Arrow => "a '->'",
      | TokenKind::As => "the 'as' keyword",
      | TokenKind::Assign => "a '='",
      | TokenKind::Async => "the 'async' keyword",
      | TokenKind::Await => "the 'await' keyword",
      | TokenKind::Bang => "a '!'",
      | TokenKind::BitAnd => "a '&'",
      | TokenKind::BitAndAssign => "a '&='",
      | TokenKind::BitOr => "a '|'",
      | TokenKind::BitOrAssign => "a '|='",
      | TokenKind::BitXor => "a '^'",
      | TokenKind::BitXorAssign => "a '^='",
      | TokenKind::BraceClose => "a '}'",
      | TokenKind::BraceOpen => "a '{'",
      | TokenKind::BracketClose => "a ']'",
      | TokenKind::BracketOpen => "an '['",
      | TokenKind::Break => "the 'break' keyword",
      | TokenKind::Builtin => "the 'builtin' keyword",
      | TokenKind::Case => "the 'case' keyword",
      | TokenKind::Colon => "a ':'",
      | TokenKind::Comma => "a ','",
      | TokenKind::Comment => "a comment",
      | TokenKind::Const => "the 'const' keyword",
      | TokenKind::Div => "a '/'",
      | TokenKind::DivAssign => "a '/='",
      | TokenKind::Dot => "a '.'",
      | TokenKind::DoubleArrow => "a '=>'",
      | TokenKind::DoubleColon => "a '::'",
      | TokenKind::DoubleStringClose => "a '\"'",
      | TokenKind::DoubleStringOpen => "a '\"'",
      | TokenKind::Else => "the 'else' keyword",
      | TokenKind::Enum => "the 'enum' keyword",
      | TokenKind::Eq => "a '=='",
      | TokenKind::Extern => "the 'extern' keyword",
      | TokenKind::False => "the 'false' keyword",
      | TokenKind::Float => "a float",
      | TokenKind::Fn => "the 'fn' keyword",
      | TokenKind::For => "the 'for' keyword",
      | TokenKind::Ge => "a '>='",
      | TokenKind::Gt => "a '>'",
      | TokenKind::Identifier => "an identifier",
      | TokenKind::If => "the 'if' keyword",
      | TokenKind::Impl => "the 'impl' keyword",
      | TokenKind::Integer => "an integer",
      | TokenKind::Invalid => "an invalid token",
      | TokenKind::InvalidEscape => "an invalid Unicode escape sequence",
      | TokenKind::Iso => "the 'iso' keyword",
      | TokenKind::Le => "a '<='",
      | TokenKind::Let => "the 'let' keyword",
      | TokenKind::Loop => "the 'loop' keyword",
      | TokenKind::Lt => "a '<'",
      | TokenKind::Match => "the 'match' keyword",
      | TokenKind::Mod => "a '%'",
      | TokenKind::ModAssign => "a '%='",
      | TokenKind::Move => "the 'move' keyword",
      | TokenKind::Mul => "a '*'",
      | TokenKind::MulAssign => "a '*='",
      | TokenKind::Mut => "the 'mut' keyword",
      | TokenKind::Ne => "a '!='",
      | TokenKind::Next => "the 'next' keyword",
      | TokenKind::Null => "the end of the input",
      | TokenKind::Or => "an '||'",
      | TokenKind::ParenClose => "a closing parenthesis",
      | TokenKind::ParenOpen => "an opening parenthesis",
      | TokenKind::Pow => "a '**'",
      | TokenKind::PowAssign => "a '**='",
      | TokenKind::Pub => "the 'pub' keyword",
      | TokenKind::Recover => "the 'recover' keyword",
      | TokenKind::Ref => "the 'ref' keyword",
      | TokenKind::Replace => "a '=:'",
      | TokenKind::Return => "the 'return' keyword",
      | TokenKind::SelfType => "the 'Self' keyword",
      | TokenKind::SelfValue => "the 'self' keyword",
      | TokenKind::Shl => "a '<<'",
      | TokenKind::ShlAssign => "a '<<='",
      | TokenKind::Shr => "a '>>'",
      | TokenKind::ShrAssign => "a '>>='",
      | TokenKind::SingleStringClose => "a '''",
      | TokenKind::SingleStringOpen => "a '''",
      | TokenKind::StringExprClose => "a closing curly brace",
      | TokenKind::StringExprOpen => "an opening curly brace",
      | TokenKind::StringText => "the text of a string",
      | TokenKind::Struct => "the 'struct' keyword",
      | TokenKind::Sub => "a '-'",
      | TokenKind::SubAssign => "a '-='",
      | TokenKind::Trait => "the 'trait' keyword",
      | TokenKind::True => "the 'true' keyword",
      | TokenKind::Try => "the 'try' keyword",
      | TokenKind::TryOp => "the '?' postfix operator",
      | TokenKind::UnicodeEscape => "an Unicode escape sequence",
      | TokenKind::UnsignedShr => "a '>>>'",
      | TokenKind::UnsignedShrAssign => "a '>>>='",
      | TokenKind::Use => "the 'use' keyword",
      | TokenKind::While => "the 'while' keyword",
      | TokenKind::Whitespace => "whitespace",
    }
  }
}

#[derive(PartialEq, Eq, Debug)]
pub struct Token {
  pub kind: TokenKind,
  pub value: String,
  pub span: Span,
}

impl Token {
  pub fn new(kind: TokenKind, value: String, span: Span) -> Self {
    Self { kind, value, span }
  }

  /// Returns a token signalling unexpected input. The token contains the invalid character.
  pub fn invalid(value: String, span: Span) -> Self {
    Self::new(TokenKind::Invalid, value, span)
  }

  /// Returns a token that signals the end of the input stream. Using null tokens so we don't need
  /// to wrap/unwrap every token in [Option].
  pub fn null(span: Span) -> Self {
    Self::new(TokenKind::Null, String::new(), span)
  }

  pub fn is_keyword(&self) -> bool {
    matches!(
      self.kind,
      TokenKind::As
        | TokenKind::Async
        | TokenKind::Await
        | TokenKind::Break
        | TokenKind::Builtin
        | TokenKind::Case
        | TokenKind::Const
        | TokenKind::Else
        | TokenKind::Enum
        | TokenKind::Extern
        | TokenKind::False
        | TokenKind::Fn
        | TokenKind::For
        | TokenKind::If
        | TokenKind::Impl
        | TokenKind::Iso
        | TokenKind::Let
        | TokenKind::Loop
        | TokenKind::Match
        | TokenKind::Move
        | TokenKind::Mut
        | TokenKind::Next
        | TokenKind::Pub
        | TokenKind::Recover
        | TokenKind::Ref
        | TokenKind::Return
        | TokenKind::SelfType
        | TokenKind::SelfValue
        | TokenKind::Struct
        | TokenKind::Trait
        | TokenKind::True
        | TokenKind::Try
        | TokenKind::Use
        | TokenKind::While
    )
  }

  pub fn is_operator(&self) -> bool {
    matches!(
      self.kind,
      TokenKind::Add
        | TokenKind::And
        | TokenKind::Bang
        | TokenKind::BitAnd
        | TokenKind::BitOr
        | TokenKind::BitXor
        | TokenKind::Div
        | TokenKind::Eq
        | TokenKind::Ge
        | TokenKind::Gt
        | TokenKind::Le
        | TokenKind::Lt
        | TokenKind::Mod
        | TokenKind::Mul
        | TokenKind::Ne
        | TokenKind::Or
        | TokenKind::Pow
        | TokenKind::Shl
        | TokenKind::Shr
        | TokenKind::Sub
        | TokenKind::TryOp
        | TokenKind::UnsignedShr
    )
  }

  pub fn same_line_as(&self, token: &Token) -> bool {
    self.span.lines.start() == token.span.lines.start()
  }
}
