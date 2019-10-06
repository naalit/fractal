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
}

impl std::fmt::Display for Node<Term> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &**self {
            Term::Lit(l) => l.fmt(f),
            Term::Rec(s) => write!(f, "rec {}", INTERN.read().unwrap().resolve(*s).unwrap()),
            Term::Var(s) => f.write_str(INTERN.read().unwrap().resolve(*s).unwrap()),
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
                f.write_str("fun");
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
    pub modules: HashMap<Type, HashMap<Sym, T>>,
}
impl<T: Typeable> Env<T> {
    pub fn get_ty(&self, s: Sym) -> Option<Type> {
        self.env.get(&s).map(|x| x.ty(self))
    }
}
impl<T> Env<T> {
    pub fn get(&self, s: Sym) -> Option<&T> {
        self.env.get(&s)
    }
    pub fn insert(&mut self, s: Sym, v: T) {
        self.env.insert(s, v);
    }
}

/// A nominal type, which is mostly for use as a namespace
/// All this stuff will probably be replaced when we decide how type namespaces will actually work
#[derive(PartialEq, Hash, Eq, Debug, Clone)]
pub enum Type {
    /// A built-in number type
    Num,
    Record,
    /// A function with the given return type
    Fun(Box<Type>),
    Tuple(Box<Type>, Box<Type>),
    /// This is a recursive call, so it's whatever the parent thinks it is (based on the base case)
    Rec,
    /// The type could be unknown, or it might just not have one
    None,
}

pub trait Typeable {
    fn ty(&self, env: &Env<impl Typeable>) -> Type;
    // This shouldn't be called recursively, and it gives Rec a chance to resolve
    fn ty_once(&self, env: &Env<impl Typeable>) -> Type {
        self.ty(env)
    }
}
impl Typeable for Term {
    fn ty_once(&self, env: &Env<impl Typeable>) -> Type {
        match self {
            Term::Rec(s) => env.get_ty(*s).unwrap_or(Type::None),
            Term::Var(s) => env.get_ty(*s).unwrap_or(Type::None),
            Term::Rec(s) => Type::Rec,
            Term::Dot(x, s) => env
                .modules
                .get(&x.ty_once(env))
                .and_then(|m| m.get(s))
                .map(|x| x.ty_once(env))
                .unwrap_or(Type::None),
            Term::Lit(Literal::Int(_)) | Term::Lit(Literal::Float(_)) => Type::Num,
            Term::Tuple(a, b) => Type::Tuple(Box::new(a.ty_once(env)), Box::new(b.ty_once(env))),
            Term::App(a, _b) => match a.ty_once(env) {
                Type::Fun(x) => *x,
                y => y,
            },
            Term::Fun(x) => type_fun(&x, env),
            _ => Type::None,
        }
    }
    fn ty(&self, env: &Env<impl Typeable>) -> Type {
        match self {
            Term::Var(s) => env.get_ty(*s).unwrap_or(Type::None),
            Term::Rec(s) => Type::Rec,
            Term::Dot(x, s) => match x.ty(env) {
                Type::Rec => Type::Rec,
                t => env
                    .modules
                    .get(&t)
                    .and_then(|m| m.get(s))
                    .map(|x| x.ty(env))
                    .unwrap_or(Type::None),
            },
            Term::Lit(Literal::Int(_)) | Term::Lit(Literal::Float(_)) => Type::Num,
            Term::Tuple(a, b) => Type::Tuple(Box::new(a.ty(env)), Box::new(b.ty(env))),
            Term::App(a, _b) => match a.ty(env) {
                Type::Fun(x) => *x,
                Type::Rec => Type::Rec,
                y => y,
            },
            Term::Fun(x) => type_fun(&x, env),
            _ => Type::None,
        }
    }
}
impl<T: Typeable> Typeable for Node<T> {
    fn ty_once(&self, env: &Env<impl Typeable>) -> Type {
        self.val.ty_once(env)
    }
    fn ty(&self, env: &Env<impl Typeable>) -> Type {
        self.val.ty(env)
    }
}

pub fn type_fun(x: &[Fun], env: &Env<impl Typeable>) -> Type {
    let mut tys = x.iter().map(|x| x.rhs.ty(env));
    let mut t = tys.next().unwrap().clone();
    for i in tys {
        if t == Type::Rec {
            t = i;
        } else if i != Type::Rec && i != t {
            return Type::Fun(Box::new(Type::None));
        }
    }
    Type::Fun(Box::new(t))
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
    Var(Sym),
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
