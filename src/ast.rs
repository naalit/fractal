use crate::common::*;
use std::collections::HashMap;
use std::rc::Rc;

/// An AST node, with error reporting information attached
/// Note that the Clone implementation is a shallow clone, it has an Rc inside
#[derive(PartialEq, Clone)]
pub struct Node<T> {
    pub file: codespan::FileId,
    pub span: codespan::Span,
    pub val: Rc<T>,
}

impl<T> Node<T> {
    /// Uses a zero span in the first registered file
    pub fn new_raw(val: T) -> Node<T> {
        Node {
            span: codespan::Span::default(),
            file: *NO_FILE,
            val: Rc::new(val),
        }
    }
    pub fn new(span: codespan::Span, file: codespan::FileId, val: T) -> Node<T> {
        Node {
            span,
            file,
            val: Rc::new(val),
        }
    }
    pub fn replace<U>(&self, val: U) -> Node<U> {
        Node {
            val: Rc::new(val),
            span: self.span,
            file: self.file,
        }
    }
}

impl std::fmt::Display for Node<Term> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &**self {
            Term::Lit(l) => l.fmt(f),
            Term::Rec(s) => write!(f, "rec {}", INTERN.read().unwrap().resolve(*s).unwrap()),
            Term::Var(s) => write!(f, "var {}", INTERN.read().unwrap().resolve(*s).unwrap()),
            Term::Ident(s) => f.write_str(INTERN.read().unwrap().resolve(*s).unwrap()),
            Term::Tuple(a, b) => write!(f, "{}, {}", a, b),
            Term::Union(a, b) => write!(f, "{} | {}", a, b),
            Term::Inter(a, b) => write!(f, "{} : {}", a, b),
            Term::Dot(a, s) => write!(f, "{}.{}", a, INTERN.read().unwrap().resolve(*s).unwrap()),
            Term::Block(v) => {
                f.write_str("do")?;
                for i in v {
                    // TODO make indentation work in nested blocks
                    f.write_str("\n")?;
                    // TODO fix indentation
                    f.write_fmt(format_args!("{:indent$}{}", "", i, indent = 2))?;
                }
                Ok(())
            }
            Term::App(a, b) => write!(f, "{}({})", a, b),
            Term::Fun(v) => {
                f.write_str("fun")?;
                for i in v {
                    // TODO make indentation work in nested blocks
                    write!(f, "\n{:indent$}{} => {}", "", i.lhs, i.rhs, indent = 2)?;
                }
                Ok(())
            }
            Term::Def(a, b) => write!(f, "{} = {}", a, b),
        }
    }
}

impl<T> std::ops::Deref for Node<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.val
    }
}
impl<T: std::fmt::Debug> std::fmt::Debug for Node<T> {
    /// We don't care that it's wrapped in a Node, so this just calls Term::fmt()
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.val.fmt(f)
    }
}

#[derive(Clone)]
pub struct Env<T> {
    pub env: HashMap<Sym, T>,
}
impl<T> Env<T> {
    pub fn get(&self, s: Sym) -> Option<&T> {
        self.env.get(&s)
    }
    pub fn insert(&mut self, s: Sym, v: T) {
        self.env.insert(s, v);
    }
}

pub type BTerm = Node<Term>;

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Literal {
    Int(i32),
    Float(f32),
    Nil,
}

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Literal::Int(i) => f.write_fmt(format_args!("{}", i)),
            Literal::Float(i) => f.write_fmt(format_args!("{}", i)),
            Literal::Nil => f.write_str("()"),
        }
    }
}

/// A term, the entire language is representable
#[derive(Debug, PartialEq, Clone)]
pub enum Term {
    Lit(Literal),
    Rec(Sym),
    /// 'var x'
    Var(Sym),
    /// 'x'
    Ident(Sym),
    Tuple(BTerm, BTerm),
    Union(BTerm, BTerm),
    /// The Intersection of two patterns - represented with `:`
    Inter(BTerm, BTerm),
    Dot(BTerm, Sym),
    Block(Vec<BTerm>),
    App(BTerm, BTerm),
    Fun(Vec<Fun>),
    Def(BTerm, BTerm),
}

/// Represents a match arm in a `fun`
#[derive(Debug, PartialEq, Clone)]
pub struct Fun {
    pub lhs: BTerm,
    pub rhs: BTerm,
}
