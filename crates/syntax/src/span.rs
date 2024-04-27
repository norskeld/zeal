use std::cmp::{Ord, Ordering, PartialOrd};
use std::fmt;
use std::ops::Range;

/// The location span of a single expression.
#[derive(PartialEq, Eq, Clone)]
pub struct Span {
  /// The first and last line of the expression.
  pub lines: Range<usize>,
  /// The first and last column of the expression.
  pub cols: Range<usize>,
}

impl Span {
  pub fn new(lines: Range<usize>, cols: Range<usize>) -> Self {
    Self { lines, cols }
  }

  /// Returns the location span of the start and end of the expression.
  pub fn start_end(start: &Self, end: &Self) -> Self {
    Self {
      lines: start.lines.start..end.lines.end,
      cols: start.cols.start..end.cols.end,
    }
  }

  /// Returns starting line and column of the location span.
  pub fn line_column(&self) -> (usize, usize) {
    (self.lines.start, self.cols.start)
  }
}

impl fmt::Debug for Span {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(
      f,
      "lns {}..{}, cols {}..{}",
      self.lines.start, self.lines.end, self.cols.start, self.cols.end
    )
  }
}

impl PartialOrd for Span {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

impl Ord for Span {
  fn cmp(&self, other: &Self) -> Ordering {
    let ord = self.lines.start.cmp(&other.lines.start);

    if ord == Ordering::Equal {
      self.cols.start.cmp(&other.cols.start)
    } else {
      ord
    }
  }
}
