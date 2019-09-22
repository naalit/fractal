//! This module is going to be rewritten entirely later, because right now it's a really slow tree-walking interpreter
//! So, there aren't any tests, and it's not well written

use crate::ast::*;
use crate::pattern::*;
use codespan::{FileId, Span};
use std::rc::Rc;
use string_interner::Sym;

#[derive(Clone, Debug, PartialEq)]
pub enum Builtin {
    Print,
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Fun(Rc<Vec<Fun>>),
    Int(i32),
    Float(f32),
    Partial(Box<Value>, Box<Value>),
    Tuple(Box<Value>, Box<Value>),
    Builtin(Builtin),
    Nil,
}
impl Typeable for Value {
    fn ty(&self, env: &Env<impl Typeable>) -> Type {
        match self {
            Value::Int(_) | Value::Float(_) => Type::Num,
            Value::Tuple(a, b) => Type::Tuple(Box::new(a.ty(env)), Box::new(b.ty(env))),
            Value::Builtin(b) => match b {
                Builtin::Add | Builtin::Div | Builtin::Mul | Builtin::Sub => {
                    Type::Fun(Box::new(Type::Num))
                }
                Builtin::Print => Type::Fun(Box::new(Type::None)),
            },
            Value::Partial(a, _) => match a.ty(env) {
                Type::Fun(x) => *x,
                x => x,
            },
            _ => Type::None,
        }
    }
}
impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Fun(_) => write!(f, "<function>"),
            Value::Int(i) => write!(f, "{}", i),
            Value::Float(i) => write!(f, "{}", i),
            Value::Tuple(a, b) => write!(f, "({}, {})", a, b),
            Value::Builtin(b) => write!(f, "{:?}", b),
            Value::Partial(a, b) => write!(f, "{}({})", a, b),
            Value::Nil => write!(f, "nil"),
        }
    }
}

pub struct EvalContext {
    env: Env<Value>,
}

#[derive(Debug)]
pub struct EvalError {
    pub span: Span,
    pub file: FileId,
    pub val: ErrorType,
}
impl EvalError {
    pub fn new(span: Span, file: FileId, val: ErrorType) -> Self {
        EvalError { span, file, val }
    }
}

#[derive(Debug)]
pub enum ErrorType {
    NotFound(Sym),
    MemberNotFound(Type, Sym),
    UnImplemented,
    MatchError,
}

impl EvalContext {
    pub fn new(intern: &crate::parse::Intern) -> Self {
        let env = vec![(
            intern.borrow_mut().get_or_intern("print"),
            Value::Builtin(Builtin::Print),
        )]
        .into_iter()
        .collect();
        let modules = vec![(
            Type::Num,
            vec![
                ("+", Value::Builtin(Builtin::Add)),
                ("-", Value::Builtin(Builtin::Sub)),
                ("*", Value::Builtin(Builtin::Mul)),
                ("/", Value::Builtin(Builtin::Div)),
            ]
            .into_iter()
            .map(|(a, b)| (intern.borrow_mut().get_or_intern(a), b))
            .collect(),
        )]
        .into_iter()
        .collect();
        EvalContext {
            env: Env { env, modules },
        }
    }

    /// Run the tree-walking interpreter on `node`
    pub fn eval(&mut self, node: Node) -> Result<Value, EvalError> {
        let Node { span, file, val } = node;
        match val {
            Term::Dot(a, s) => {
                let ty = a.ty(&self.env);
                match self.env.modules.get(&ty).and_then(|x| x.get(&s)) {
                    Some(x) => Ok(Value::Partial(
                        Box::new(x.clone()),
                        Box::new(self.eval(*a)?),
                    )),
                    None => Err(EvalError::new(span, file, ErrorType::MemberNotFound(ty, s))),
                }
            }
            Term::Var(s) => match self.env.get(&s) {
                Some(v) => Ok(v.clone()),
                None => Err(EvalError::new(span, file, ErrorType::NotFound(s))),
            },
            Term::Int(i) => Ok(Value::Int(i)),
            Term::Float(f) => Ok(Value::Float(f)),
            Term::Def(s, v) => match s.val {
                Term::Var(s) => {
                    let v = self.eval(*v)?;
                    self.env.insert(s, v);
                    Ok(Value::Nil)
                }
                _ => Err(EvalError::new(span, file, ErrorType::UnImplemented)),
            },
            Term::Tuple(a, b) => {
                let a = self.eval(*a)?;
                let b = self.eval(*b)?;
                Ok(Value::Tuple(Box::new(a), Box::new(b)))
            }
            Term::App(a, b) => match self.eval(*a)? {
                Value::Fun(arms) => {
                    let b = self.eval(*b)?;
                    let arm = arms
                        .iter()
                        .map(|x| (&x.rhs, x.lhs.match_strict(&b)))
                        .find(|(b, x)| x.is_pass());
                    match arm {
                        Some((body, MatchResult::Pass(sub))) => {
                            let mut old = Vec::new();
                            for (k, v) in sub {
                                if let Some(v) = self.env.env.insert(k.clone(), v) {
                                    old.push((k, v));
                                }
                            }

                            let r = self.eval(body.clone());

                            for (k, v) in old {
                                self.env.env.insert(k.clone(), v);
                            }

                            r
                        }
                        _ => Err(EvalError::new(span, file, ErrorType::MatchError)),
                    }
                }
                Value::Builtin(Builtin::Print) => {
                    println!("{}", self.eval(*b)?);
                    Ok(Value::Nil)
                }
                Value::Partial(f, a) => self.builtin_partial(span, file, *f, *a, *b),
                _ => Err(EvalError::new(span, file, ErrorType::UnImplemented)),
            },
            Term::Fun(f) => Ok(Value::Fun(Rc::new(f))),
            _ => Err(EvalError::new(span, file, ErrorType::UnImplemented)),
        }
    }

    /// Evaluate the 2-ary builtin `f` on `a` and `b` (e.g. arithmetic)
    /// On a failure, assumes `span` and `file` represent the span of the builtin application
    fn builtin_partial(
        &mut self,
        span: Span,
        file: FileId,
        f: Value,
        a: Value,
        b: Node,
    ) -> Result<Value, EvalError> {
        match f {
            Value::Builtin(Builtin::Add) => {
                let b = self.eval(b)?;
                match (a, b) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
                    _ => Err(EvalError::new(span, file, ErrorType::UnImplemented)),
                }
            }
            Value::Builtin(Builtin::Sub) => {
                let b = self.eval(b)?;
                match (a, b) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
                    _ => Err(EvalError::new(span, file, ErrorType::UnImplemented)),
                }
            }
            Value::Builtin(Builtin::Mul) => {
                let b = self.eval(b)?;
                match (a, b) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
                    _ => Err(EvalError::new(span, file, ErrorType::UnImplemented)),
                }
            }
            Value::Builtin(Builtin::Div) => {
                let b = self.eval(b)?;
                match (a, b) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a / b)),
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a / b)),
                    _ => Err(EvalError::new(span, file, ErrorType::UnImplemented)),
                }
            }
            _ => Err(EvalError::new(span, file, ErrorType::UnImplemented)),
        }
    }
}
