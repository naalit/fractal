pub use crate::ast::{Literal, Node};
pub use crate::vm::Value;
pub use string_interner::Sym;

use codespan::{FileId, Files};
use lazy_static::lazy_static;
use std::sync::RwLock;
use string_interner::StringInterner;

// There are a lot of statics here, which could be considered bad practice
// However, we're only going to use one of each of these things,
// even if we compile two programs at once or something. They're just helpers.
lazy_static! {
    pub static ref FILES: RwLock<Files> = RwLock::new(Files::new());
    /// Used for empty spans
    pub static ref NO_FILE: FileId = FILES.write().unwrap().add("<builtin>", "");

    pub static ref INTERN: RwLock<StringInterner<Sym>> = RwLock::new(StringInterner::new());
}
