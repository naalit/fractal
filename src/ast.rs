use std::collections::HashMap;
use string_interner::Sym;

/// An AST node, with error reporting information attached
#[derive(Debug)]
pub struct Node {
    pub file: codespan::FileId,
    pub span: codespan::Span,
    pub val: Term,
}
impl Node {
    pub fn format(&self, intern: &crate::parse::Intern) -> String {
        match &self.val {
            Term::Var(s) => intern.borrow().resolve(*s).unwrap().to_string(),
            Term::Int(i) => i.to_string(),
            Term::Float(f) => f.to_string(),
            Term::Tuple(a, b) => format!("{}, {}", a.format(intern), b.format(intern)).to_string(),
            Term::Union(a, b) => format!("{} | {}", a.format(intern), b.format(intern)).to_string(),
            Term::Dot(a, s) => format!(
                "{}.{}",
                a.format(intern),
                intern.borrow().resolve(*s).unwrap()
            )
            .to_string(),
            Term::Block(v) => format!("do {:#?}", v),
            Term::App(a, b) => format!("{}({})", a.format(intern), b.format(intern)).to_string(),
            Term::Fun(v) => format!("do {:#?}", v),
            Term::Def(a, b) => format!("{} = {}", a.format(intern), b.format(intern)).to_string(),
        }
    }
}

pub struct Env<T> {
    pub env: HashMap<Sym, T>,
    pub modules: HashMap<Type, HashMap<Sym, T>>,
}
impl<T: Typeable> Env<T> {
    pub fn get_ty(&self, s: &Sym) -> Option<Type> {
        self.env.get(s).map(|x| x.ty(self))
    }
}
impl<T> Env<T> {
    pub fn get(&self, s: &Sym) -> Option<&T> {
        self.env.get(s)
    }
    pub fn insert(&mut self, s: Sym, v: T) {
        self.env.insert(s, v);
    }
}

/// A nominal type, which is mostly for use as a namespace
#[derive(PartialEq, Hash, Eq, Debug)]
pub enum Type {
    /// A built-in number type
    Num,
    Record,
    /// A function with the given return type
    Fun(Box<Type>),
    Tuple(Box<Type>, Box<Type>),
    /// The type could be unknown, or it might just not have one
    None,
}

pub trait Typeable {
    fn ty(&self, env: &Env<impl Typeable>) -> Type;
}
impl Typeable for Term {
    fn ty(&self, env: &Env<impl Typeable>) -> Type {
        match self {
            Term::Var(s) => env.get_ty(s).unwrap_or(Type::None),
            Term::Dot(x, s) => env
                .modules
                .get(&x.ty(env))
                .and_then(|m| m.get(s))
                .map(|x| x.ty(env))
                .unwrap_or(Type::None),
            Term::Int(_) | Term::Float(_) => Type::Num,
            Term::Tuple(a, b) => Type::Tuple(Box::new(a.ty(env)), Box::new(b.ty(env))),
            Term::App(a, _b) => match a.ty(env) {
                Type::Fun(x) => *x,
                y => y,
            },
            _ => Type::None,
        }
    }
}
impl Typeable for Node {
    fn ty(&self, env: &Env<impl Typeable>) -> Type {
        self.val.ty(env)
    }
}

/// The actual thing that's inside a `Node`
#[derive(Debug)]
pub enum Term {
    Var(Sym),
    Int(i32),
    Float(f32),
    Tuple(Box<Node>, Box<Node>),
    Union(Box<Node>, Box<Node>),
    Dot(Box<Node>, Sym),
    Block(Vec<Node>),
    App(Box<Node>, Box<Node>),
    Fun(Vec<Fun>),
    Def(Box<Node>, Box<Node>),
}

/// Represents a match arm in a `fun`
#[derive(Debug)]
pub struct Fun {
    pub lhs: Node,
    pub rhs: Node,
}

/// In the future may add spans to the methods
pub trait Visitor {
    fn visit_var(&mut self, _var: Sym) {}
    fn visit_int(&mut self, _int: i32) {}
    fn visit_float(&mut self, _float: f32) {}

    fn start_tuple(&mut self) {}
    fn end_tuple(&mut self) {}

    fn start_union(&mut self) {}
    fn end_union(&mut self) {}

    fn start_block(&mut self) {}
    fn end_block(&mut self) {}

    fn start_app(&mut self) {}
    fn end_app(&mut self) {}

    fn start_fun(&mut self) {}
    fn end_fun(&mut self) {}
    fn next_arm(&mut self) {}

    fn def_rhs(&mut self) {}
    fn def_lhs(&mut self) {}
    fn end_def(&mut self) {}

    // You probably shouldn't override this
    // fn visit(&mut self, node: &'a Node) {
    //     match &node.val {
    //         Term::Var(v) => self.visit_var(*v),
    //         Term::Int(i) => self.visit_int(*i),
    //         Term::Float(f) => self.visit_float(*f),
    //         Term::Tuple(v) => if v.len() == 1 {
    //             self.visit(&v[0])
    //         } else {
    //             self.visit_tuple(v)
    //         },
    //         Term::Union(v) => if v.len() == 1 {
    //             self.visit(&v[0])
    //         } else {
    //             self.visit_union(v)
    //         },
    //         Term::Block(v) => self.visit_block(v),
    //         Term::App(v) => self.visit_app(v),
    //         Term::Fun(f) => self.visit_fun(f),
    //         Term::Def(l, r) => self.visit_def(l, r),
    //     }
    // }
}
