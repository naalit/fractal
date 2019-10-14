//! This stuff is mostly for writing tests

mod ast;
mod builtin;
mod common;
mod error;
mod parse;
mod pattern;

use crate::pattern::HasTotal;
pub use ast::Literal;
pub use common::*;
pub use pattern::{BTotal, Total};

#[derive(Debug)]
pub enum RunError {
    ParseError(error::Error),
    MatchError(pattern::MatchError),
}

pub fn run(src: impl Into<String>) -> Result<BTotal, RunError> {
    let p = parse::parse_str("<test>", src).map_err(RunError::ParseError)?;
    if p.len() != 1 {
        panic!("Wrong number of terms parsed, expected 1, got {}", p.len());
    }
    p[0].total(&mut builtin::totals(), false)
        .map_err(RunError::MatchError)
}

/// Works like assert_eq!, but the second argument can be any pattern (like Term::Var(_))
/// `assert_match!(@ok e, p)` will `.unwrap()` and dereference e, for Options or Results of boxes
#[macro_export]
macro_rules! assert_match {
    (@ok $e:expr, $p:pat) => {
        match &**$e.as_ref().unwrap() {
            $p => (),
            _ => panic!(
                "assert_match! failed, expected {:?}, got {:?}",
                stringify!($p),
                $e
            ),
        }
    };
    ($e:expr, $p:pat) => {
        match $e {
            $p => (),
            _ => panic!(
                "assert_match! failed, expected {:?}, got {:?}",
                stringify!($p),
                $e
            ),
        }
    };
}
