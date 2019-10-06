//! This module is going to be rewritten entirely later, because right now it's a really slow tree-walking interpreter
//! So, there aren't any tests, and it's not well written

use crate::ast::*;
use crate::builtin::*;
use crate::common::*;
use crate::pattern::*;
use codespan::{FileId, Span};
use std::rc::Rc;

/// A runtime value, a subset of terms
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Fun(Rc<Vec<Fun>>),
    Lit(Literal),
    Partial(Box<Value>, Box<Value>),
    Tuple(Box<Value>, Box<Value>),
    Builtin(Builtin),
}
impl Match for Value {
    fn match_strict(&self, other: &Value, env: &Env<impl Match>) -> MatchResult {
        MatchResult::Fail
    }
}
impl Value {
    pub const Nil: Value = Value::Lit(Literal::Nil);
}
impl Typeable for Value {
    fn ty(&self, env: &Env<impl Typeable>) -> Type {
        match self {
            Value::Lit(Literal::Int(_)) | Value::Lit(Literal::Float(_)) => Type::Num,
            Value::Tuple(a, b) => Type::Tuple(Box::new(a.ty(env)), Box::new(b.ty(env))),
            Value::Builtin(b) => match b {
                Builtin::Add | Builtin::Div | Builtin::Mul | Builtin::Sub => {
                    Type::Fun(Box::new(Type::Num))
                }
                Builtin::Sqr => Type::Fun(Box::new(Type::Num)),
                Builtin::Print => Type::Fun(Box::new(Type::None)),
                Builtin::Num => Type::None,
            },
            Value::Partial(a, _) => match a.ty(env) {
                Type::Fun(x) => *x,
                x => x,
            },
            Value::Fun(x) => type_fun(&x, env),
            _ => Type::None,
        }
    }
}
impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Fun(_) => write!(f, "<function>"),
            Value::Lit(i) => write!(f, "{}", i),
            Value::Tuple(a, b) => write!(f, "({}, {})", a, b),
            Value::Builtin(b) => write!(f, "{:?}", b),
            Value::Partial(a, b) => write!(f, "{}({})", a, b),
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
    pub fn new() -> Self {
        let env = values();
        EvalContext { env }
    }

    /// Run the tree-walking interpreter on `node`
    pub fn eval(&mut self, node: BTerm) -> Result<Value, EvalError> {
        let Node { span, file, val } = node;
        match &*val {
            Term::Dot(a, s) => {
                let ty = a.ty_once(&self.env);
                match self.env.modules.get(&ty).and_then(|x| x.get(&s)) {
                    Some(x) => Ok(Value::Partial(
                        Box::new(x.clone()),
                        Box::new(self.eval(a.clone())?),
                    )),
                    None => Err(EvalError::new(
                        span,
                        file,
                        ErrorType::MemberNotFound(ty, *s),
                    )),
                }
            }
            Term::Var(s) | Term::Rec(s) => match self.env.get(*s) {
                Some(v) => Ok(v.clone()),
                None => Err(EvalError::new(span, file, ErrorType::NotFound(*s))),
            },
            Term::Lit(i) => Ok(Value::Lit(*i)),
            Term::Def(s, v) => match s.match_strict(&self.eval(v.clone())?, &self.env) {
                MatchResult::Pass(v) => {
                    self.env.env.extend(v);
                    Ok(Value::Nil)
                }
                _ => Err(EvalError::new(span, file, ErrorType::MatchError)),
            },
            Term::Tuple(a, b) => {
                let a = self.eval(a.clone())?;
                let b = self.eval(b.clone())?;
                Ok(Value::Tuple(Box::new(a), Box::new(b)))
            }
            Term::App(a, b) => match self.eval(a.clone())? {
                Value::Fun(arms) => {
                    let b = self.eval(b.clone())?;
                    let arm = arms
                        .iter()
                        .map(|x| (&x.rhs, x.lhs.match_strict(&b, &self.env)))
                        .find(|(_, m)| m.is_pass());
                    match arm {
                        Some((body, MatchResult::Pass(sub))) => {
                            let mut old = Vec::new();
                            for (k, v) in sub {
                                if let Some(v) = self.env.env.insert(k, v) {
                                    old.push((k, v));
                                }
                            }

                            let r = self.eval(body.clone());

                            for (k, v) in old {
                                self.env.env.insert(k, v);
                            }

                            r
                        }
                        _ => Err(EvalError::new(span, file, ErrorType::MatchError)),
                    }
                }
                Value::Builtin(Builtin::Print) => {
                    println!("{}", self.eval(b.clone())?);
                    Ok(Value::Nil)
                }
                Value::Builtin(Builtin::Sqr) => match self.eval(b.clone())? {
                    Value::Lit(Literal::Int(i)) => Ok(Value::Lit(Literal::Int(i * i))),
                    Value::Lit(Literal::Float(f)) => Ok(Value::Lit(Literal::Float(f * f))),
                    _ => Err(EvalError::new(span, file, ErrorType::MatchError)),
                },
                Value::Partial(f, a) => self.builtin_partial(span, file, *f, *a, b.clone()),
                _ => Err(EvalError::new(span, file, ErrorType::UnImplemented)),
            },
            Term::Fun(f) => Ok(Value::Fun(Rc::new(f.clone()))),
            Term::Block(v) => {
                let old = self.env.clone();
                let mut last = Value::Nil;
                for i in v {
                    last = self.eval(i.clone())?;
                }
                self.env = old;
                Ok(last)
            }
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
        b: BTerm,
    ) -> Result<Value, EvalError> {
        let b = self.eval(b)?;
        if let (Value::Lit(a), Value::Lit(b)) = (a, b) {
            let l = match f {
                Value::Builtin(Builtin::Add) => match (a, b) {
                    (Literal::Int(a), Literal::Int(b)) => Ok(Literal::Int(a + b)),
                    (Literal::Float(a), Literal::Float(b)) => Ok(Literal::Float(a + b)),
                    _ => Err(EvalError::new(span, file, ErrorType::MatchError)),
                },
                Value::Builtin(Builtin::Sub) => match (a, b) {
                    (Literal::Int(a), Literal::Int(b)) => Ok(Literal::Int(a - b)),
                    (Literal::Float(a), Literal::Float(b)) => Ok(Literal::Float(a - b)),
                    _ => Err(EvalError::new(span, file, ErrorType::MatchError)),
                },
                Value::Builtin(Builtin::Mul) => match (a, b) {
                    (Literal::Int(a), Literal::Int(b)) => Ok(Literal::Int(a * b)),
                    (Literal::Float(a), Literal::Float(b)) => Ok(Literal::Float(a * b)),
                    _ => Err(EvalError::new(span, file, ErrorType::MatchError)),
                },
                Value::Builtin(Builtin::Div) => match (a, b) {
                    (Literal::Int(a), Literal::Int(b)) => Ok(Literal::Int(a / b)),
                    (Literal::Float(a), Literal::Float(b)) => Ok(Literal::Float(a / b)),
                    _ => Err(EvalError::new(span, file, ErrorType::MatchError)),
                },
                _ => Err(EvalError::new(span, file, ErrorType::UnImplemented)),
            }?;
            Ok(Value::Lit(l))
        } else {
            Err(EvalError::new(span, file, ErrorType::UnImplemented))
        }
    }
}
