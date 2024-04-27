//! Lexical analysis, tokenizer.

use unicode_segmentation::UnicodeSegmentation;

use crate::ascii::*;
use crate::span::Span;
use crate::token::{Token, TokenKind};

/// Mapping of string escape sequences to their replacement bytes.
struct EscapeMap {
  mapping: [Option<u8>; 128],
}

impl EscapeMap {
  /// Create an empty escape map.
  const fn new() -> Self {
    Self {
      mapping: [None; 128],
    }
  }

  /// Create mapping.
  const fn map(mut self, byte: u8, to: u8) -> Self {
    self.mapping[byte as usize] = Some(to);
    self
  }

  /// Resolve mapping.
  const fn get(&self, index: u8) -> Option<u8> {
    let idx = index as usize;

    if idx < self.mapping.len() {
      return self.mapping[idx];
    }

    None
  }
}

/// The escape sequence literals supported by a single quoted string, and their replacement bytes.
const SINGLE_ESCAPES: EscapeMap = EscapeMap::new()
  .map(SINGLE_QUOTE, SINGLE_QUOTE)
  .map(BACKSLASH, BACKSLASH);

/// The escape sequence literals supported by a double quoted string, and their replacement bytes.
const DOUBLE_ESCAPES: EscapeMap = EscapeMap::new()
  .map(DOUBLE_QUOTE, DOUBLE_QUOTE)
  .map(SINGLE_QUOTE, SINGLE_QUOTE)
  .map(ZERO, NULL)
  .map(BACKSLASH, BACKSLASH)
  .map(LOWER_E, ESCAPE)
  .map(LOWER_N, NEWLINE)
  .map(LOWER_R, CARRIAGE_RETURN)
  .map(LOWER_T, TAB)
  .map(BRACE_OPEN, BRACE_OPEN);

/// Scanning state to help handle string interpolation and line continuation.
#[derive(Copy, Clone)]
enum State {
  Default,
  SingleString,
  DoubleString,
}

pub struct Scanner {
  /// The stream of bytes to process.
  input: Vec<u8>,
  /// The maximum position in the input stream.
  max_position: usize,
  /// The current position in the input stream.
  position: usize,
  /// The number of opening braces that have yet to be closed.
  braces: usize,
  /// The stack of brace counts to use for determining when a string expression should be closed.
  braces_stack: Vec<usize>,
  /// The stack of scanning states.
  states: Vec<State>,
  /// The current line number.
  line: usize,
  /// The current (starting) column number.
  column: usize,
}

impl Scanner {
  pub fn new(input: Vec<u8>) -> Self {
    let max = input.len();

    Self {
      input,
      max_position: max,
      position: 0,
      braces: 0,
      braces_stack: vec![],
      states: vec![State::Default],
      line: 1,
      column: 1,
    }
  }

  pub fn initial_span(&self) -> Span {
    Span::new(self.line..self.line, self.column..self.column)
  }

  pub fn next_token(&mut self) -> Token {
    match self.states.last().cloned() {
      | Some(State::SingleString) => self.next_single_string_token(),
      | Some(State::DoubleString) => self.next_double_string_token(),
      | _ => self.next_ordinary_token(),
    }
  }

  #[inline]
  fn enter_state(&mut self, state: State) {
    self.states.push(state);
  }

  #[inline]
  fn leave_state(&mut self) {
    self.states.pop();
  }

  fn span(&self, start_line: usize, start_column: usize) -> Span {
    Span::new(start_line..self.line, start_column..self.column)
  }

  fn current_byte(&self) -> u8 {
    if self.has_next() {
      self.input[self.position]
    } else {
      0
    }
  }

  fn next_byte(&self) -> u8 {
    self.peek(1)
  }

  fn peek(&self, offset: usize) -> u8 {
    let index = self.position + offset;

    if index < self.max_position {
      self.input[index]
    } else {
      0
    }
  }

  fn advance_line(&mut self) {
    self.position += 1;
    self.column = 1;
    self.line += 1;
  }

  fn advance_column(&mut self, value: &str) {
    self.column += value.graphemes(true).count();
  }

  fn advance_char(&mut self) {
    self.column += 1;
    self.position += 1;
  }

  fn has_next(&self) -> bool {
    self.position < self.max_position
  }

  fn next_double_string_token(&mut self) -> Token {
    match self.current_byte() {
      | DOUBLE_QUOTE => {
        self.leave_state();
        self.single_character_token(TokenKind::DoubleStringClose)
      },
      | BACKSLASH if self.next_is_unicode_escape() => self.unicode_escape_token(),
      | BRACE_OPEN => self.double_string_expression_open(),
      | _ if self.has_next() => self.double_string_text(),
      | _ => self.null(),
    }
  }

  fn next_single_string_token(&mut self) -> Token {
    match self.current_byte() {
      | SINGLE_QUOTE => {
        self.leave_state();
        self.single_character_token(TokenKind::SingleStringClose)
      },
      | _ if self.has_next() => self.single_string_text(),
      | _ => self.null(),
    }
  }

  fn unicode_escape_token(&mut self) -> Token {
    let mut buffer = Vec::new();
    let mut is_closed = false;

    let line = self.line;
    let column = self.column;

    // Advance three characters for the `\u{`.
    self.position += 3;
    self.column += 3;

    while self.has_next() {
      let byte = self.current_byte();

      if byte == BRACE_CLOSE {
        is_closed = true;
        self.advance_char();
        break;
      }

      if byte == DOUBLE_QUOTE {
        break;
      }

      self.position += 1;

      buffer.push(byte);
    }

    let mut kind = TokenKind::InvalidEscape;
    let mut value = String::from_utf8_lossy(&buffer).into_owned();

    self.advance_column(&value);

    let span = self.span(line, column);

    if is_closed && !value.is_empty() && value.len() <= 6 {
      let hex = u32::from_str_radix(&value, 16)
        .ok()
        .and_then(char::from_u32)
        .map(String::from);

      if let Some(parsed) = hex {
        kind = TokenKind::UnicodeEscape;
        value = parsed;
      }
    }

    Token::new(kind, value, span)
  }

  fn next_ordinary_token(&mut self) -> Token {
    match self.current_byte() {
      | ZERO..=NINE => self.number(false),
      | BRACE_OPEN => self.brace_open(),
      | BRACE_CLOSE => self.brace_close(),
      | PAREN_OPEN => self.paren_open(),
      | PAREN_CLOSE => self.paren_close(),
      | SINGLE_QUOTE => self.single_quote(),
      | DOUBLE_QUOTE => self.double_quote(),
      | COLON => self.colon(),
      | PERCENT => self.percent(),
      | SLASH => {
        if self.peek(1) == SLASH {
          self.comment()
        } else {
          self.slash()
        }
      },
      | QUESTION => self.try_op(),
      | CARET => self.caret(),
      | AMPERSAND => self.ampersand(),
      | PIPE => self.pipe(),
      | STAR => self.star(),
      | MINUS => self.minus(),
      | PLUS => self.plus(),
      | EQUAL => self.equal(),
      | LESS => self.less(),
      | GREATER => self.greater(),
      | BRACKET_OPEN => self.bracket_open(),
      | BRACKET_CLOSE => self.bracket_close(),
      | BANG => self.bang(),
      | DOT => self.dot(),
      | COMMA => self.comma(),
      | UNDERSCORE => self.underscore(),
      | LOWER_A..=LOWER_Z | UPPER_A..=UPPER_Z => self.identifier_or_keyword(self.position),
      | SPACE | TAB | CARRIAGE_RETURN | NEWLINE => self.whitespace(),
      | _ => {
        if self.has_next() {
          self.invalid(self.position, self.position + 1)
        } else {
          self.null()
        }
      },
    }
  }

  fn whitespace(&mut self) -> Token {
    let start = self.position;
    let line = self.line;
    let column = self.column;

    let mut new_line = false;

    while self.has_next() {
      match self.current_byte() {
        | SPACE | TAB | CARRIAGE_RETURN => self.advance_char(),
        | NEWLINE => {
          new_line = true;
          self.advance_char();

          break;
        },
        | _ => break,
      }
    }

    let value = self.slice(start, self.position);
    let span = self.span(line, column);

    if new_line {
      self.column = 1;
      self.line += 1;
    }

    Token::new(TokenKind::Whitespace, value, span)
  }

  fn number(&mut self, skip_first: bool) -> Token {
    let start = self.position;
    let line = self.line;

    if skip_first {
      self.position += 1;
    }

    let first = self.current_byte();
    let second = self.next_byte();

    let mut kind = TokenKind::Integer;

    if first == ZERO && (second == LOWER_X || second == UPPER_X) {
      // Advance 2 for "0x"
      self.position += 2;

      while let ZERO..=NINE | LOWER_A..=LOWER_F | UPPER_A..=UPPER_F | UNDERSCORE =
        self.current_byte()
      {
        self.position += 1;
      }

      return self.token(kind, start, line);
    }

    loop {
      match self.current_byte() {
        | ZERO..=NINE | UNDERSCORE => {},
        | LOWER_E | UPPER_E => {
          match self.next_byte() {
            // 10e5, 10E5, etc
            | ZERO..=NINE => {
              kind = TokenKind::Float;
            },
            // 10e+5, 10e-5, etc
            | PLUS | MINUS if (ZERO..=NINE).contains(&self.peek(2)) => {
              self.position += 1;
              kind = TokenKind::Float;
            },
            | _ => break,
          }
        },
        | DOT if (ZERO..=NINE).contains(&self.next_byte()) => {
          kind = TokenKind::Float;
        },
        | _ => break,
      }

      self.position += 1;
    }

    self.token(kind, start, line)
  }

  fn comment(&mut self) -> Token {
    let column = self.column;
    let line = self.line;

    self.advance_char();
    self.advance_char();

    // The first space in a comment is ignored, so that a comment doesn't start with a space.
    if self.current_byte() == SPACE {
      self.advance_char();
    }

    let start = self.position;

    while self.has_next() && self.current_byte() != NEWLINE {
      self.position += 1;
    }

    let comment = self.token_with_column(TokenKind::Comment, start, line, column);

    self.advance_line();

    comment
  }

  fn brace_open(&mut self) -> Token {
    self.braces += 1;

    self.single_character_token(TokenKind::BraceOpen)
  }

  fn brace_close(&mut self) -> Token {
    if self.braces > 0 {
      self.braces -= 1;
    }

    let count = self.braces_stack.last().cloned();

    if count == Some(self.braces) {
      self.braces_stack.pop();
      self.leave_state();

      self.single_character_token(TokenKind::StringExprClose)
    } else {
      self.single_character_token(TokenKind::BraceClose)
    }
  }

  fn paren_open(&mut self) -> Token {
    self.single_character_token(TokenKind::ParenOpen)
  }

  fn paren_close(&mut self) -> Token {
    self.single_character_token(TokenKind::ParenClose)
  }

  fn single_quote(&mut self) -> Token {
    self.enter_state(State::SingleString);
    self.single_character_token(TokenKind::SingleStringOpen)
  }

  fn double_quote(&mut self) -> Token {
    self.enter_state(State::DoubleString);
    self.single_character_token(TokenKind::DoubleStringOpen)
  }

  fn colon(&mut self) -> Token {
    let start = self.position;
    let line = self.line;

    let (increment, kind) = match self.next_byte() {
      | COLON => (2, TokenKind::DoubleColon),
      | EQUAL => (2, TokenKind::Replace),
      | _ => (1, TokenKind::Colon),
    };

    self.position += increment;

    self.token(kind, start, line)
  }

  fn try_op(&mut self) -> Token {
    let start = self.position;
    let line = self.line;

    self.position += 1;

    self.token(TokenKind::TryOp, start, line)
  }

  fn percent(&mut self) -> Token {
    self.operator(TokenKind::Mod, TokenKind::ModAssign, self.position)
  }

  fn slash(&mut self) -> Token {
    self.operator(TokenKind::Div, TokenKind::DivAssign, self.position)
  }

  fn caret(&mut self) -> Token {
    self.operator(TokenKind::BitXor, TokenKind::BitXorAssign, self.position)
  }

  fn ampersand(&mut self) -> Token {
    if self.next_byte() == AMPERSAND {
      let start = self.position;
      self.position += 2;

      return self.token(TokenKind::And, start, self.line);
    }

    self.operator(TokenKind::BitAnd, TokenKind::BitAndAssign, self.position)
  }

  fn pipe(&mut self) -> Token {
    if self.next_byte() == PIPE {
      let start = self.position;
      self.position += 2;

      return self.token(TokenKind::Or, start, self.line);
    }

    self.operator(TokenKind::BitOr, TokenKind::BitOrAssign, self.position)
  }

  fn star(&mut self) -> Token {
    if self.next_byte() == STAR {
      return self.double_operator(TokenKind::Pow, TokenKind::PowAssign);
    }

    self.operator(TokenKind::Mul, TokenKind::MulAssign, self.position)
  }

  fn minus(&mut self) -> Token {
    match self.next_byte() {
      | ZERO..=NINE => self.number(true),
      | GREATER => self.arrow(),
      | _ => self.operator(TokenKind::Sub, TokenKind::SubAssign, self.position),
    }
  }

  fn plus(&mut self) -> Token {
    self.operator(TokenKind::Add, TokenKind::AddAssign, self.position)
  }

  fn arrow(&mut self) -> Token {
    let start = self.position;
    self.position += 2;

    self.token(TokenKind::Arrow, start, self.line)
  }

  fn equal(&mut self) -> Token {
    let start = self.position;

    let (increment, kind) = match self.next_byte() {
      | EQUAL => (2, TokenKind::Eq),
      | GREATER => (2, TokenKind::DoubleArrow),
      | COLON => (2, TokenKind::Replace),
      | _ => (1, TokenKind::Assign),
    };

    self.position += increment;

    self.token(kind, start, self.line)
  }

  fn less(&mut self) -> Token {
    if self.next_byte() == LESS {
      return self.double_operator(TokenKind::Shl, TokenKind::ShlAssign);
    }

    self.operator(TokenKind::Lt, TokenKind::Le, self.position)
  }

  fn greater(&mut self) -> Token {
    if self.next_byte() == GREATER {
      return if self.peek(2) == GREATER {
        self.triple_operator(TokenKind::UnsignedShr, TokenKind::UnsignedShrAssign)
      } else {
        self.double_operator(TokenKind::Shr, TokenKind::ShrAssign)
      };
    }

    self.operator(TokenKind::Gt, TokenKind::Ge, self.position)
  }

  fn bracket_open(&mut self) -> Token {
    self.single_character_token(TokenKind::BracketOpen)
  }

  fn bracket_close(&mut self) -> Token {
    self.single_character_token(TokenKind::BracketClose)
  }

  fn bang(&mut self) -> Token {
    let start = self.position;

    if self.next_byte() == EQUAL {
      self.position += 2;

      return self.token(TokenKind::Ne, start, self.line);
    }

    self.position += 1;

    self.token(TokenKind::Bang, start, self.line)
  }

  fn dot(&mut self) -> Token {
    let start = self.position;
    let line = self.line;

    self.position += 1;

    self.token(TokenKind::Dot, start, line)
  }

  fn comma(&mut self) -> Token {
    self.single_character_token(TokenKind::Comma)
  }

  fn identifier_or_keyword(&mut self, start: usize) -> Token {
    let column = self.column;

    self.advance_identifier_bytes();

    let value = self.slice(start, self.position);

    // TODO: This can be improved by using a proper trie.
    // https://craftinginterpreters.com/scanning-on-demand.html#tries-and-state-machines
    let kind = match value.len() {
      | 2 => {
        match value.as_str() {
          | "as" => TokenKind::As,
          | "fn" => TokenKind::Fn,
          | "if" => TokenKind::If,
          | _ => TokenKind::Identifier,
        }
      },
      | 3 => {
        match value.as_str() {
          | "for" => TokenKind::For,
          | "iso" => TokenKind::Iso,
          | "let" => TokenKind::Let,
          | "mut" => TokenKind::Mut,
          | "pub" => TokenKind::Pub,
          | "ref" => TokenKind::Ref,
          | "try" => TokenKind::Try,
          | "use" => TokenKind::Use,
          | _ => TokenKind::Identifier,
        }
      },
      | 4 => {
        match value.as_str() {
          | "case" => TokenKind::Case,
          | "else" => TokenKind::Else,
          | "enum" => TokenKind::Enum,
          | "impl" => TokenKind::Impl,
          | "loop" => TokenKind::Loop,
          | "move" => TokenKind::Move,
          | "next" => TokenKind::Next,
          | "self" => TokenKind::SelfValue,
          | "Self" => TokenKind::SelfType,
          | "true" => TokenKind::True,
          | _ => TokenKind::Identifier,
        }
      },
      | 5 => {
        match value.as_str() {
          | "async" => TokenKind::Async,
          | "await" => TokenKind::Await,
          | "break" => TokenKind::Break,
          | "const" => TokenKind::Const,
          | "false" => TokenKind::False,
          | "match" => TokenKind::Match,
          | "trait" => TokenKind::Trait,
          | "while" => TokenKind::While,
          | _ => TokenKind::Identifier,
        }
      },
      | 6 => {
        match value.as_str() {
          | "extern" => TokenKind::Extern,
          | "return" => TokenKind::Return,
          | "struct" => TokenKind::Struct,
          | _ => TokenKind::Identifier,
        }
      },
      | 7 => {
        match value.as_str() {
          | "builtin" => TokenKind::Builtin,
          | "recover" => TokenKind::Recover,
          | _ => TokenKind::Identifier,
        }
      },
      | _ => TokenKind::Identifier,
    };

    self.advance_column(&value);

    Token::new(kind, value, self.span(self.line, column))
  }

  fn underscore(&mut self) -> Token {
    let start = self.position;

    while self.current_byte() == UNDERSCORE {
      self.position += 1;
    }

    self.identifier_or_keyword(start)
  }

  fn single_string_text(&mut self) -> Token {
    let kind = TokenKind::StringText;

    let mut buffer = Vec::new();
    let mut new_line = false;

    let line = self.line;
    let column = self.column;

    while self.has_next() {
      match self.current_byte() {
        | BACKSLASH => {
          let next = self.next_byte();

          if self.replace_escape_sequence(&mut buffer, next, &SINGLE_ESCAPES) {
            continue;
          }

          buffer.push(BACKSLASH);

          self.position += 1;
        },
        | NEWLINE => {
          new_line = true;
          buffer.push(NEWLINE);

          break;
        },
        | SINGLE_QUOTE => {
          break;
        },
        | byte => {
          buffer.push(byte);

          self.position += 1;
        },
      }
    }

    self.string_text_token(kind, buffer, line, column, new_line)
  }

  fn double_string_text(&mut self) -> Token {
    let kind = TokenKind::StringText;

    let mut buffer = Vec::new();
    let mut new_line = false;

    let line = self.line;
    let column = self.column;

    while self.has_next() {
      match self.current_byte() {
        | BACKSLASH => {
          if self.next_is_unicode_escape() {
            break;
          }

          let next = self.next_byte();

          if self.replace_escape_sequence(&mut buffer, next, &DOUBLE_ESCAPES) {
            continue;
          }

          buffer.push(BACKSLASH);

          self.position += 1;
        },
        | NEWLINE => {
          new_line = true;
          buffer.push(NEWLINE);

          break;
        },
        | DOUBLE_QUOTE | BRACE_OPEN => {
          break;
        },
        | byte => {
          buffer.push(byte);

          self.position += 1;
        },
      }
    }

    self.string_text_token(kind, buffer, line, column, new_line)
  }

  fn double_string_expression_open(&mut self) -> Token {
    self.enter_state(State::Default);

    self.braces_stack.push(self.braces);
    self.braces += 1;

    self.single_character_token(TokenKind::StringExprOpen)
  }

  fn replace_escape_sequence(
    &mut self,
    buffer: &mut Vec<u8>,
    byte: u8,
    replacements: &EscapeMap,
  ) -> bool {
    if let Some(replace) = replacements.get(byte) {
      buffer.push(replace);

      // The replacement is included in the buffer, meaning we'll also include it for advancing
      // column numbers. As such we only need to advance for the backslash here.
      self.column += 1;
      self.position += 2;

      true
    } else {
      false
    }
  }

  fn string_text_token(
    &mut self,
    kind: TokenKind,
    buffer: Vec<u8>,
    line: usize,
    column: usize,
    new_line: bool,
  ) -> Token {
    let value = String::from_utf8_lossy(&buffer).into_owned();

    if !value.is_empty() {
      self.column += value.graphemes(true).count();
    }

    let span = self.span(line, column);

    if new_line {
      self.advance_line();
    }

    Token::new(kind, value, span)
  }

  fn advance_identifier_bytes(&mut self) {
    while let ZERO..=NINE | LOWER_A..=LOWER_Z | UPPER_A..=UPPER_Z | UNDERSCORE | DOLLAR =
      self.current_byte()
    {
      self.position += 1
    }
  }

  fn next_is_unicode_escape(&self) -> bool {
    self.next_byte() == LOWER_U && self.peek(2) == BRACE_OPEN
  }

  fn triple_operator(&mut self, kind: TokenKind, assign_kind: TokenKind) -> Token {
    let start = self.position;
    self.position += 2;

    self.operator(kind, assign_kind, start)
  }

  fn double_operator(&mut self, kind: TokenKind, assign_kind: TokenKind) -> Token {
    let start = self.position;
    self.position += 1;

    self.operator(kind, assign_kind, start)
  }

  fn operator(&mut self, kind: TokenKind, assign_kind: TokenKind, start: usize) -> Token {
    let mut token_kind = kind;
    let mut increment = 1;

    if self.next_byte() == EQUAL {
      token_kind = assign_kind;
      increment = 2;
    }

    self.position += increment;

    let value = self.slice(start, self.position);
    let line = self.line;
    let column = self.column;

    self.advance_column(&value);

    Token::new(token_kind, value, self.span(line, column))
  }

  fn token_with_column(
    &mut self,
    kind: TokenKind,
    start: usize,
    line: usize,
    column: usize,
  ) -> Token {
    let value = self.slice(start, self.position);

    self.advance_column(&value);

    let span = self.span(line, column);

    Token::new(kind, value, span)
  }

  fn token(&mut self, kind: TokenKind, start: usize, line: usize) -> Token {
    self.token_with_column(kind, start, line, self.column)
  }

  fn single_character_token(&mut self, kind: TokenKind) -> Token {
    let start = self.position;
    let line = self.line;

    self.position += 1;

    self.token(kind, start, line)
  }

  fn slice(&mut self, start: usize, stop: usize) -> String {
    String::from_utf8_lossy(&self.input[start..stop]).into_owned()
  }

  fn invalid(&mut self, start: usize, stop: usize) -> Token {
    let col = self.column;
    let value = self.slice(start, stop);

    self.advance_column(&value);

    let span = self.span(self.line, col);

    // TODO: Right now, when we run into invalid input we immediately stop scanning. We should
    // be able to continue scanning, i.e. recover from errors.
    self.position = self.max_position;

    Token::invalid(value, span)
  }

  fn null(&self) -> Token {
    // When we encounter the end of the input, we want the location to point to the last column that
    // came before it. This way any errors are reported within the bounds of the column range.
    let lines = self.line..self.line;

    let span = if self.column == 1 {
      Span::new(lines, 1..1)
    } else {
      Span::new(lines, self.column..self.column)
    };

    Token::null(span)
  }
}

#[cfg(test)]
mod tests {
  use std::ops::Range;

  use pretty_assertions::assert_eq;

  use super::TokenKind::*;
  use super::*;

  fn scanner(input: &str) -> Scanner {
    Scanner::new(Vec::from(input))
  }

  fn span(lines: Range<usize>, cols: Range<usize>) -> Span {
    Span::new(lines, cols)
  }

  fn token(kind: TokenKind, value: &str, lines: Range<usize>, cols: Range<usize>) -> Token {
    Token::new(kind, value.to_string(), span(lines, cols))
  }

  macro_rules! assert_token {
    (
      $input: expr,
      $kind: expr,
      $value: expr,
      $lines: expr,
      $cols: expr
    ) => {{
      let mut scanner = scanner($input);
      let next = scanner.next_token();

      assert_eq!(next, token($kind, $value, $lines, $cols))
    }};
  }

  macro_rules! assert_tokens {
    (
      $input: expr,
      $($token: expr),+
    ) => {{
      let mut scanner = scanner($input);
      let mut tokens = Vec::new();
      let mut next = scanner.next_token();

      while next.kind != TokenKind::Null {
        tokens.push(next);

        next = scanner.next_token();
      }

      assert_eq!(tokens, vec![$( $token, )+]);
    }};
  }

  #[test]
  fn test_is_keyword() {
    assert!(token(TokenKind::As, "", 1..1, 1..1).is_keyword());
    assert!(token(TokenKind::Async, "", 1..1, 1..1).is_keyword());
    assert!(token(TokenKind::Break, "", 1..1, 1..1).is_keyword());
    assert!(token(TokenKind::Builtin, "", 1..1, 1..1).is_keyword());
    assert!(token(TokenKind::Case, "", 1..1, 1..1).is_keyword());
    assert!(token(TokenKind::Else, "", 1..1, 1..1).is_keyword());
    assert!(token(TokenKind::Enum, "", 1..1, 1..1).is_keyword());
    assert!(token(TokenKind::False, "", 1..1, 1..1).is_keyword());
    assert!(token(TokenKind::Fn, "", 1..1, 1..1).is_keyword());
    assert!(token(TokenKind::For, "", 1..1, 1..1).is_keyword());
    assert!(token(TokenKind::If, "", 1..1, 1..1).is_keyword());
    assert!(token(TokenKind::Impl, "", 1..1, 1..1).is_keyword());
    assert!(token(TokenKind::Iso, "", 1..1, 1..1).is_keyword());
    assert!(token(TokenKind::Let, "", 1..1, 1..1).is_keyword());
    assert!(token(TokenKind::Loop, "", 1..1, 1..1).is_keyword());
    assert!(token(TokenKind::Match, "", 1..1, 1..1).is_keyword());
    assert!(token(TokenKind::Move, "", 1..1, 1..1).is_keyword());
    assert!(token(TokenKind::Mut, "", 1..1, 1..1).is_keyword());
    assert!(token(TokenKind::Next, "", 1..1, 1..1).is_keyword());
    assert!(token(TokenKind::Pub, "", 1..1, 1..1).is_keyword());
    assert!(token(TokenKind::Recover, "", 1..1, 1..1).is_keyword());
    assert!(token(TokenKind::Ref, "", 1..1, 1..1).is_keyword());
    assert!(token(TokenKind::Return, "", 1..1, 1..1).is_keyword());
    assert!(token(TokenKind::SelfValue, "", 1..1, 1..1).is_keyword());
    assert!(token(TokenKind::Struct, "", 1..1, 1..1).is_keyword());
    assert!(token(TokenKind::Trait, "", 1..1, 1..1).is_keyword());
    assert!(token(TokenKind::True, "", 1..1, 1..1).is_keyword());
    assert!(token(TokenKind::Try, "", 1..1, 1..1).is_keyword());
    assert!(token(TokenKind::Use, "", 1..1, 1..1).is_keyword());
    assert!(token(TokenKind::While, "", 1..1, 1..1).is_keyword());
  }

  #[test]
  fn test_is_operator() {
    assert!(token(TokenKind::Add, "", 1..1, 1..1).is_operator());
    assert!(token(TokenKind::And, "", 1..1, 1..1).is_operator());
    assert!(token(TokenKind::BitAnd, "", 1..1, 1..1).is_operator());
    assert!(token(TokenKind::BitOr, "", 1..1, 1..1).is_operator());
    assert!(token(TokenKind::BitXor, "", 1..1, 1..1).is_operator());
    assert!(token(TokenKind::Div, "", 1..1, 1..1).is_operator());
    assert!(token(TokenKind::Eq, "", 1..1, 1..1).is_operator());
    assert!(token(TokenKind::Ge, "", 1..1, 1..1).is_operator());
    assert!(token(TokenKind::Gt, "", 1..1, 1..1).is_operator());
    assert!(token(TokenKind::Le, "", 1..1, 1..1).is_operator());
    assert!(token(TokenKind::Lt, "", 1..1, 1..1).is_operator());
    assert!(token(TokenKind::Mod, "", 1..1, 1..1).is_operator());
    assert!(token(TokenKind::Mul, "", 1..1, 1..1).is_operator());
    assert!(token(TokenKind::Ne, "", 1..1, 1..1).is_operator());
    assert!(token(TokenKind::Or, "", 1..1, 1..1).is_operator());
    assert!(token(TokenKind::Pow, "", 1..1, 1..1).is_operator());
    assert!(token(TokenKind::Shl, "", 1..1, 1..1).is_operator());
    assert!(token(TokenKind::Shr, "", 1..1, 1..1).is_operator());
    assert!(token(TokenKind::Sub, "", 1..1, 1..1).is_operator());
    assert!(token(TokenKind::UnsignedShr, "", 1..1, 1..1).is_operator());
  }

  #[test]
  fn test_token_same_line_as() {
    let token1 = token(TokenKind::As, "", 1..1, 1..1);
    let token2 = token(TokenKind::As, "", 1..1, 1..1);
    let token3 = token(TokenKind::As, "", 2..2, 1..1);

    assert!(token1.same_line_as(&token2));
    assert!(!token1.same_line_as(&token3));
  }

  #[test]
  fn test_integer() {
    assert_token!("10", Integer, "10", 1..1, 1..3);
    assert_token!("10x", Integer, "10", 1..1, 1..3);
    assert_token!("10_20_30", Integer, "10_20_30", 1..1, 1..9);
    assert_token!("0xaf", Integer, "0xaf", 1..1, 1..5);
    assert_token!("0xFF", Integer, "0xFF", 1..1, 1..5);
    assert_token!("0xF_F", Integer, "0xF_F", 1..1, 1..6);
    assert_token!("10Ea", Integer, "10", 1..1, 1..3);
    assert_token!("10.+5", Integer, "10", 1..1, 1..3);
  }

  #[test]
  fn test_float() {
    assert_token!("10.5", Float, "10.5", 1..1, 1..5);
    assert_token!("10.5x", Float, "10.5", 1..1, 1..5);
    assert_token!("10e5", Float, "10e5", 1..1, 1..5);
    assert_token!("10E5", Float, "10E5", 1..1, 1..5);
    assert_token!("10.2e5", Float, "10.2e5", 1..1, 1..7);
    assert_token!("1_0.2e5", Float, "1_0.2e5", 1..1, 1..8);
    assert_token!("10e+5", Float, "10e+5", 1..1, 1..6);
    assert_token!("10e-5", Float, "10e-5", 1..1, 1..6);
    assert_token!("10E+5", Float, "10E+5", 1..1, 1..6);
    assert_token!("10E-5", Float, "10E-5", 1..1, 1..6);
    assert_token!("1_000_000_000.0", Float, "1_000_000_000.0", 1..1, 1..16);
  }

  #[test]
  fn test_comment() {
    assert_token!("//foo", Comment, "foo", 1..1, 1..6);
    assert_token!("// foo", Comment, "foo", 1..1, 1..7);
    assert_token!("// foo\nbar", Comment, "foo", 1..1, 1..7);
    assert_token!("// €€€", Comment, "€€€", 1..1, 1..7);
  }

  #[test]
  fn test_braces() {
    assert_token!("{", BraceOpen, "{", 1..1, 1..2);
    assert_token!("}", BraceClose, "}", 1..1, 1..2);
  }

  #[test]
  fn test_brace_balancing() {
    let mut lexer = scanner("{}");

    assert_eq!(lexer.next_token(), token(BraceOpen, "{", 1..1, 1..2));
    assert_eq!(lexer.next_token(), token(BraceClose, "}", 1..1, 2..3));
  }

  #[test]
  fn test_parentheses() {
    assert_token!("(", ParenOpen, "(", 1..1, 1..2);
    assert_token!(")", ParenClose, ")", 1..1, 1..2);
  }

  #[test]
  fn test_parentheses_balancing() {
    let mut scanner = scanner("()");

    assert_eq!(scanner.next_token(), token(ParenOpen, "(", 1..1, 1..2));
    assert_eq!(scanner.next_token(), token(ParenClose, ")", 1..1, 2..3));
  }

  #[test]
  fn text_scanner_whitespace() {
    assert_token!("\t", Whitespace, "\t", 1..1, 1..2);
    assert_token!(" ", Whitespace, " ", 1..1, 1..2);
    assert_token!("\r", Whitespace, "\r", 1..1, 1..2);
    assert_tokens!(
      " 10 \t\r",
      token(Whitespace, " ", 1..1, 1..2),
      token(Integer, "10", 1..1, 2..4),
      token(Whitespace, " \t\r", 1..1, 4..7)
    );
    assert_tokens!(
      "\n10\n",
      token(Whitespace, "\n", 1..1, 1..2),
      token(Integer, "10", 2..2, 1..3),
      token(Whitespace, "\n", 2..2, 3..4)
    );
    assert_tokens!(
      " \n ",
      token(Whitespace, " \n", 1..1, 1..3),
      token(Whitespace, " ", 2..2, 1..2)
    );
  }

  #[test]
  fn test_single_quoted_string() {
    assert_tokens!(
      "''",
      token(SingleStringOpen, "'", 1..1, 1..2),
      token(SingleStringClose, "'", 1..1, 2..3)
    );
    assert_tokens!(
      "'foo'",
      token(SingleStringOpen, "'", 1..1, 1..2),
      token(StringText, "foo", 1..1, 2..5),
      token(SingleStringClose, "'", 1..1, 5..6)
    );
    assert_tokens!(
      "'\nfoo'",
      token(SingleStringOpen, "'", 1..1, 1..2),
      token(StringText, "\n", 1..1, 2..3),
      token(StringText, "foo", 2..2, 1..4),
      token(SingleStringClose, "'", 2..2, 4..5)
    );
    assert_tokens!(
      "'foo\nbar'",
      token(SingleStringOpen, "'", 1..1, 1..2),
      token(StringText, "foo\n", 1..1, 2..6),
      token(StringText, "bar", 2..2, 1..4),
      token(SingleStringClose, "'", 2..2, 4..5)
    );
    assert_tokens!(
      "'foo\n'",
      token(SingleStringOpen, "'", 1..1, 1..2),
      token(StringText, "foo\n", 1..1, 2..6),
      token(SingleStringClose, "'", 2..2, 1..2)
    );
    assert_tokens!(
      "'foo\n '",
      token(SingleStringOpen, "'", 1..1, 1..2),
      token(StringText, "foo\n", 1..1, 2..6),
      token(StringText, " ", 2..2, 1..2),
      token(SingleStringClose, "'", 2..2, 2..3)
    );
    assert_tokens!(
      "'foo\\xbar'",
      token(SingleStringOpen, "'", 1..1, 1..2),
      token(StringText, "foo\\xbar", 1..1, 2..10),
      token(SingleStringClose, "'", 1..1, 10..11)
    );
    assert_tokens!(
      "'foo\\'bar'",
      token(SingleStringOpen, "'", 1..1, 1..2),
      token(StringText, "foo\'bar", 1..1, 2..10),
      token(SingleStringClose, "'", 1..1, 10..11)
    );
    assert_tokens!(
      "'foo\\\\bar'",
      token(SingleStringOpen, "'", 1..1, 1..2),
      token(StringText, "foo\\bar", 1..1, 2..10),
      token(SingleStringClose, "'", 1..1, 10..11)
    );
    assert_tokens!(
      "'foo\\nbar'",
      token(SingleStringOpen, "'", 1..1, 1..2),
      token(StringText, "foo\\nbar", 1..1, 2..10),
      token(SingleStringClose, "'", 1..1, 10..11)
    );
    assert_tokens!(
      "'\u{65}\u{301}'",
      token(SingleStringOpen, "'", 1..1, 1..2),
      token(StringText, "\u{65}\u{301}", 1..1, 2..3),
      token(SingleStringClose, "'", 1..1, 3..4)
    );
    assert_tokens!("'", token(SingleStringOpen, "'", 1..1, 1..2));
    assert_tokens!(
      "'foo",
      token(SingleStringOpen, "'", 1..1, 1..2),
      token(StringText, "foo", 1..1, 2..5)
    );
  }

  #[test]
  fn test_double_quoted_string() {
    assert_tokens!(
      "\"\"",
      token(DoubleStringOpen, "\"", 1..1, 1..2),
      token(DoubleStringClose, "\"", 1..1, 2..3)
    );
    assert_tokens!(
      "\"foo\"",
      token(DoubleStringOpen, "\"", 1..1, 1..2),
      token(StringText, "foo", 1..1, 2..5),
      token(DoubleStringClose, "\"", 1..1, 5..6)
    );
    assert_tokens!(
      "\"\nfoo\"",
      token(DoubleStringOpen, "\"", 1..1, 1..2),
      token(StringText, "\n", 1..1, 2..3),
      token(StringText, "foo", 2..2, 1..4),
      token(DoubleStringClose, "\"", 2..2, 4..5)
    );
    assert_tokens!(
      "\"foo\nbar\"",
      token(DoubleStringOpen, "\"", 1..1, 1..2),
      token(StringText, "foo\n", 1..1, 2..6),
      token(StringText, "bar", 2..2, 1..4),
      token(DoubleStringClose, "\"", 2..2, 4..5)
    );
    assert_tokens!(
      "\"foo\n\"",
      token(DoubleStringOpen, "\"", 1..1, 1..2),
      token(StringText, "foo\n", 1..1, 2..6),
      token(DoubleStringClose, "\"", 2..2, 1..2)
    );
    assert_tokens!(
      "\"foo\n \"",
      token(DoubleStringOpen, "\"", 1..1, 1..2),
      token(StringText, "foo\n", 1..1, 2..6),
      token(StringText, " ", 2..2, 1..2),
      token(DoubleStringClose, "\"", 2..2, 2..3)
    );
    assert_tokens!(
      "\"foo\\xbar\"",
      token(DoubleStringOpen, "\"", 1..1, 1..2),
      token(StringText, "foo\\xbar", 1..1, 2..10),
      token(DoubleStringClose, "\"", 1..1, 10..11)
    );
    assert_tokens!(
      "\"foo\\\"bar\"",
      token(DoubleStringOpen, "\"", 1..1, 1..2),
      token(StringText, "foo\"bar", 1..1, 2..10),
      token(DoubleStringClose, "\"", 1..1, 10..11)
    );
    assert_tokens!(
      "\"foo\\\\bar\"",
      token(DoubleStringOpen, "\"", 1..1, 1..2),
      token(StringText, "foo\\bar", 1..1, 2..10),
      token(DoubleStringClose, "\"", 1..1, 10..11)
    );
    assert_tokens!(
      "\"foo\\nbar\"",
      token(DoubleStringOpen, "\"", 1..1, 1..2),
      token(StringText, "foo\nbar", 1..1, 2..10),
      token(DoubleStringClose, "\"", 1..1, 10..11)
    );
    assert_tokens!(
      "\"foo\\tbar\"",
      token(DoubleStringOpen, "\"", 1..1, 1..2),
      token(StringText, "foo\tbar", 1..1, 2..10),
      token(DoubleStringClose, "\"", 1..1, 10..11)
    );
    assert_tokens!(
      "\"foo\\rbar\"",
      token(DoubleStringOpen, "\"", 1..1, 1..2),
      token(StringText, "foo\rbar", 1..1, 2..10),
      token(DoubleStringClose, "\"", 1..1, 10..11)
    );
    assert_tokens!(
      "\"\u{65}\u{301}\"",
      token(DoubleStringOpen, "\"", 1..1, 1..2),
      token(StringText, "\u{65}\u{301}", 1..1, 2..3),
      token(DoubleStringClose, "\"", 1..1, 3..4)
    );
    assert_tokens!("\"", token(DoubleStringOpen, "\"", 1..1, 1..2));
    assert_tokens!(
      "\"foo",
      token(DoubleStringOpen, "\"", 1..1, 1..2),
      token(StringText, "foo", 1..1, 2..5)
    );
    assert_tokens!(
      "\"\\{}\"",
      token(DoubleStringOpen, "\"", 1..1, 1..2),
      token(StringText, "{}", 1..1, 2..5),
      token(DoubleStringClose, "\"", 1..1, 5..6)
    );
  }

  #[test]
  fn test_double_string_unicode_escapes() {
    assert_tokens!(
      "\"\\u{AC}\"",
      token(DoubleStringOpen, "\"", 1..1, 1..2),
      token(UnicodeEscape, "\u{AC}", 1..1, 2..8),
      token(DoubleStringClose, "\"", 1..1, 8..9)
    );
    assert_tokens!(
      "\"a\\u{AC}b\"",
      token(DoubleStringOpen, "\"", 1..1, 1..2),
      token(StringText, "a", 1..1, 2..3),
      token(UnicodeEscape, "\u{AC}", 1..1, 3..9),
      token(StringText, "b", 1..1, 9..10),
      token(DoubleStringClose, "\"", 1..1, 10..11)
    );
    assert_tokens!(
      "\"a\\u{FFFFF}b\"",
      token(DoubleStringOpen, "\"", 1..1, 1..2),
      token(StringText, "a", 1..1, 2..3),
      token(UnicodeEscape, "\u{FFFFF}", 1..1, 3..12),
      token(StringText, "b", 1..1, 12..13),
      token(DoubleStringClose, "\"", 1..1, 13..14)
    );
    assert_tokens!(
      "\"a\\u{10FFFF}b\"",
      token(DoubleStringOpen, "\"", 1..1, 1..2),
      token(StringText, "a", 1..1, 2..3),
      token(UnicodeEscape, "\u{10FFFF}", 1..1, 3..13),
      token(StringText, "b", 1..1, 13..14),
      token(DoubleStringClose, "\"", 1..1, 14..15)
    );
    assert_tokens!(
      "\"a\\u{XXXXX}b\"",
      token(DoubleStringOpen, "\"", 1..1, 1..2),
      token(StringText, "a", 1..1, 2..3),
      token(InvalidEscape, "XXXXX", 1..1, 3..12),
      token(StringText, "b", 1..1, 12..13),
      token(DoubleStringClose, "\"", 1..1, 13..14)
    );
    assert_tokens!(
      "\"a\\u{FFFFFF}b\"",
      token(DoubleStringOpen, "\"", 1..1, 1..2),
      token(StringText, "a", 1..1, 2..3),
      token(InvalidEscape, "FFFFFF", 1..1, 3..13),
      token(StringText, "b", 1..1, 13..14),
      token(DoubleStringClose, "\"", 1..1, 14..15)
    );
    assert_tokens!(
      "\"a\\u{AAAAA #}b\"",
      token(DoubleStringOpen, "\"", 1..1, 1..2),
      token(StringText, "a", 1..1, 2..3),
      token(InvalidEscape, "AAAAA #", 1..1, 3..14),
      token(StringText, "b", 1..1, 14..15),
      token(DoubleStringClose, "\"", 1..1, 15..16)
    );
    assert_tokens!(
      "\"a\\u{€}b\"",
      token(DoubleStringOpen, "\"", 1..1, 1..2),
      token(StringText, "a", 1..1, 2..3),
      token(InvalidEscape, "€", 1..1, 3..8),
      token(StringText, "b", 1..1, 8..9),
      token(DoubleStringClose, "\"", 1..1, 9..10)
    );
    assert_tokens!(
      "\"\\u{AA\"",
      token(DoubleStringOpen, "\"", 1..1, 1..2),
      token(InvalidEscape, "AA", 1..1, 2..7),
      token(DoubleStringClose, "\"", 1..1, 7..8)
    );
  }

  #[test]
  fn test_double_string_with_expressions() {
    assert_tokens!(
      "\"foo{10}baz\"",
      token(DoubleStringOpen, "\"", 1..1, 1..2),
      token(StringText, "foo", 1..1, 2..5),
      token(StringExprOpen, "{", 1..1, 5..6),
      token(Integer, "10", 1..1, 6..8),
      token(StringExprClose, "}", 1..1, 8..9),
      token(StringText, "baz", 1..1, 9..12),
      token(DoubleStringClose, "\"", 1..1, 12..13)
    );
    assert_tokens!(
      "\"{\"{10}\"}\"",
      token(DoubleStringOpen, "\"", 1..1, 1..2),
      token(StringExprOpen, "{", 1..1, 2..3),
      token(DoubleStringOpen, "\"", 1..1, 3..4),
      token(StringExprOpen, "{", 1..1, 4..5),
      token(Integer, "10", 1..1, 5..7),
      token(StringExprClose, "}", 1..1, 7..8),
      token(DoubleStringClose, "\"", 1..1, 8..9),
      token(StringExprClose, "}", 1..1, 9..10),
      token(DoubleStringClose, "\"", 1..1, 10..11)
    );
  }

  #[test]
  fn test_double_string_with_unclosed_expression() {
    assert_tokens!(
      "\"foo{10 +\"",
      token(DoubleStringOpen, "\"", 1..1, 1..2),
      token(StringText, "foo", 1..1, 2..5),
      token(StringExprOpen, "{", 1..1, 5..6),
      token(Integer, "10", 1..1, 6..8),
      token(Whitespace, " ", 1..1, 8..9),
      token(Add, "+", 1..1, 9..10),
      token(DoubleStringOpen, "\"", 1..1, 10..11)
    );
  }

  #[test]
  fn test_colon() {
    assert_token!(":", Colon, ":", 1..1, 1..2);
    assert_token!("::", DoubleColon, "::", 1..1, 1..3);
    assert_token!(":=", Replace, ":=", 1..1, 1..3);
  }

  #[test]
  fn test_percent() {
    assert_token!("%", Mod, "%", 1..1, 1..2);
    assert_token!("%=", ModAssign, "%=", 1..1, 1..3);
  }

  #[test]
  fn test_slash() {
    assert_token!("/", Div, "/", 1..1, 1..2);
    assert_token!("/=", DivAssign, "/=", 1..1, 1..3);
  }

  #[test]
  fn test_bitwise_xor() {
    assert_token!("^", BitXor, "^", 1..1, 1..2);
    assert_token!("^=", BitXorAssign, "^=", 1..1, 1..3);
  }

  #[test]
  fn test_bitwise_and() {
    assert_token!("&", BitAnd, "&", 1..1, 1..2);
    assert_token!("&=", BitAndAssign, "&=", 1..1, 1..3);
  }

  #[test]
  fn test_bitwise_or() {
    assert_token!("|", BitOr, "|", 1..1, 1..2);
    assert_token!("|=", BitOrAssign, "|=", 1..1, 1..3);
  }

  #[test]
  fn test_bool_operators() {
    assert_tokens!("||", token(Or, "||", 1..1, 1..3));
    assert_tokens!("&&", token(And, "&&", 1..1, 1..3));
  }

  #[test]
  fn test_star() {
    assert_token!("*", Mul, "*", 1..1, 1..2);
    assert_token!("*=", MulAssign, "*=", 1..1, 1..3);
    assert_token!("**", Pow, "**", 1..1, 1..3);
    assert_token!("**=", PowAssign, "**=", 1..1, 1..4);
  }

  #[test]
  fn test_minus() {
    assert_token!("-", Sub, "-", 1..1, 1..2);
    assert_token!("-=", SubAssign, "-=", 1..1, 1..3);
    assert_token!("->", Arrow, "->", 1..1, 1..3);
    assert_tokens!("-10", token(Integer, "-10", 1..1, 1..4));
    assert_tokens!("-10.5", token(Float, "-10.5", 1..1, 1..6));
    assert_tokens!(
      "10 - 20",
      token(Integer, "10", 1..1, 1..3),
      token(Whitespace, " ", 1..1, 3..4),
      token(Sub, "-", 1..1, 4..5),
      token(Whitespace, " ", 1..1, 5..6),
      token(Integer, "20", 1..1, 6..8)
    );
  }

  #[test]
  fn test_plus() {
    assert_token!("+", Add, "+", 1..1, 1..2);
    assert_token!("+=", AddAssign, "+=", 1..1, 1..3);
  }

  #[test]
  fn test_equal() {
    assert_token!("=", Assign, "=", 1..1, 1..2);
    assert_token!("==", Eq, "==", 1..1, 1..3);
    assert_token!("=>", DoubleArrow, "=>", 1..1, 1..3);
  }

  #[test]
  fn test_less() {
    assert_token!("<", Lt, "<", 1..1, 1..2);
    assert_token!("<=", Le, "<=", 1..1, 1..3);
    assert_token!("<<", Shl, "<<", 1..1, 1..3);
    assert_token!("<<=", ShlAssign, "<<=", 1..1, 1..4);
  }

  #[test]
  fn test_greater() {
    assert_token!(">", Gt, ">", 1..1, 1..2);
    assert_token!(">=", Ge, ">=", 1..1, 1..3);
    assert_token!(">>", Shr, ">>", 1..1, 1..3);
    assert_token!(">>=", ShrAssign, ">>=", 1..1, 1..4);
    assert_token!(">>>", UnsignedShr, ">>>", 1..1, 1..4);
    assert_token!(">>>=", UnsignedShrAssign, ">>>=", 1..1, 1..5);
  }

  #[test]
  fn test_brackets() {
    assert_token!("[", BracketOpen, "[", 1..1, 1..2);
    assert_token!("]", BracketClose, "]", 1..1, 1..2);
  }

  #[test]
  fn test_bang() {
    assert_token!("!", Bang, "!", 1..1, 1..2);
    assert_token!("!=", Ne, "!=", 1..1, 1..3);
  }

  #[test]
  fn test_dot() {
    assert_token!(".", Dot, ".", 1..1, 1..2);
  }

  #[test]
  fn test_comma() {
    assert_token!(",", Comma, ",", 1..1, 1..2);
  }

  #[test]
  fn test_keywords() {
    assert_token!("as", As, "as", 1..1, 1..3);
    assert_token!("fn", Fn, "fn", 1..1, 1..3);
    assert_token!("if", If, "if", 1..1, 1..3);

    assert_token!("for", For, "for", 1..1, 1..4);
    assert_token!("iso", Iso, "iso", 1..1, 1..4);
    assert_token!("let", Let, "let", 1..1, 1..4);
    assert_token!("mut", Mut, "mut", 1..1, 1..4);
    assert_token!("pub", Pub, "pub", 1..1, 1..4);
    assert_token!("ref", Ref, "ref", 1..1, 1..4);
    assert_token!("try", Try, "try", 1..1, 1..4);
    assert_token!("use", Use, "use", 1..1, 1..4);

    assert_token!("case", Case, "case", 1..1, 1..5);
    assert_token!("else", Else, "else", 1..1, 1..5);
    assert_token!("enum", Enum, "enum", 1..1, 1..5);
    assert_token!("impl", Impl, "impl", 1..1, 1..5);
    assert_token!("loop", Loop, "loop", 1..1, 1..5);
    assert_token!("move", Move, "move", 1..1, 1..5);
    assert_token!("next", Next, "next", 1..1, 1..5);
    assert_token!("Self", SelfType, "Self", 1..1, 1..5);
    assert_token!("self", SelfValue, "self", 1..1, 1..5);
    assert_token!("true", True, "true", 1..1, 1..5);

    assert_token!("async", Async, "async", 1..1, 1..6);
    assert_token!("await", Await, "await", 1..1, 1..6);
    assert_token!("break", Break, "break", 1..1, 1..6);
    assert_token!("const", Const, "const", 1..1, 1..6);
    assert_token!("false", False, "false", 1..1, 1..6);
    assert_token!("match", Match, "match", 1..1, 1..6);
    assert_token!("trait", Trait, "trait", 1..1, 1..6);
    assert_token!("while", While, "while", 1..1, 1..6);

    assert_token!("extern", Extern, "extern", 1..1, 1..7);
    assert_token!("return", Return, "return", 1..1, 1..7);
    assert_token!("struct", Struct, "struct", 1..1, 1..7);

    assert_token!("builtin", Builtin, "builtin", 1..1, 1..8);
    assert_token!("recover", Recover, "recover", 1..1, 1..8);
  }

  #[test]
  fn test_identifiers() {
    assert_token!("foo", Identifier, "foo", 1..1, 1..4);
    assert_token!("foo$bar", Identifier, "foo$bar", 1..1, 1..8);
    assert_token!("baz", Identifier, "baz", 1..1, 1..4);
    assert_token!("foo_bar", Identifier, "foo_bar", 1..1, 1..8);
    assert_token!("foo_BAR", Identifier, "foo_BAR", 1..1, 1..8);
    assert_token!("foo_123", Identifier, "foo_123", 1..1, 1..8);
    assert_token!("foo_123a", Identifier, "foo_123a", 1..1, 1..9);
    assert_token!("foo_123a_", Identifier, "foo_123a_", 1..1, 1..10);
    assert_token!("_foo_123a", Identifier, "_foo_123a", 1..1, 1..10);
    assert_token!("__foo_123a", Identifier, "__foo_123a", 1..1, 1..11);
    assert_token!("_", Identifier, "_", 1..1, 1..2);
    assert_token!("__", Identifier, "__", 1..1, 1..3);
    assert_token!("_0", Identifier, "_0", 1..1, 1..3);
    assert_token!("_9", Identifier, "_9", 1..1, 1..3);
    assert_token!("__1", Identifier, "__1", 1..1, 1..4);

    assert_token!("FOO", Identifier, "FOO", 1..1, 1..4);
    assert_token!("FOO_bar", Identifier, "FOO_bar", 1..1, 1..8);
    assert_token!("FOO_BAR", Identifier, "FOO_BAR", 1..1, 1..8);
    assert_token!("FOO_123", Identifier, "FOO_123", 1..1, 1..8);
    assert_token!("FOO_123a", Identifier, "FOO_123a", 1..1, 1..9);
    assert_token!("FOO_123a_", Identifier, "FOO_123a_", 1..1, 1..10);
    assert_token!("_FOO_123a", Identifier, "_FOO_123a", 1..1, 1..10);
    assert_token!("__FOO_123a", Identifier, "__FOO_123a", 1..1, 1..11);
  }

  #[test]
  fn test_null_empty() {
    let mut scanner = scanner("");

    assert_eq!(scanner.next_token(), token(Null, "", 1..1, 1..1));
  }

  #[test]
  fn test_null_token() {
    let mut scanner = scanner("  ");

    assert_eq!(scanner.next_token(), token(Whitespace, "  ", 1..1, 1..3));
    assert_eq!(scanner.next_token(), token(Null, "", 1..1, 3..3));
  }

  #[test]
  fn test_null_after_newline() {
    let mut scanner = scanner("\n");

    assert_eq!(scanner.next_token(), token(Whitespace, "\n", 1..1, 1..2));
    assert_eq!(scanner.next_token(), token(Null, "", 2..2, 1..1));
  }

  #[test]
  fn test_identifier_with_try_op() {
    let mut scanner = scanner("a?b");

    assert_eq!(scanner.next_token(), token(Identifier, "a", 1..1, 1..2));
    assert_eq!(scanner.next_token(), token(TryOp, "?", 1..1, 2..3));
    assert_eq!(scanner.next_token(), token(Identifier, "b", 1..1, 3..4));
  }
}
