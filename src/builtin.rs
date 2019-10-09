use crate::ast::{Env};
use crate::common::*;
use crate::pattern::{BTotal, Total, MatchError};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Builtin {
    Print,
    Sqr,
    Add,
    Sub,
    Mul,
    Div,
    /// This is 'num', not a specific number but the pattern
    Num,
}
impl Builtin {
    pub fn args(self) -> Total {
        match self {
            Builtin::Num | Builtin::Sqr => Total::Num,
            Builtin::Sub | Builtin::Add | Builtin::Mul | Builtin::Div => Total::Tuple(Node::new_raw(Total::Num), Node::new_raw(Total::Lit(Literal::Nil))),
            Builtin::Print => Total::Num,
        }
    }

    pub fn total(self) -> Total {
        match self {
            Builtin::Num => Total::Num,
            Builtin::Sub | Builtin::Add | Builtin::Mul | Builtin::Div | Builtin::Sqr => Total::Fun(vec![(Node::new_raw(Total::Num), Node::new_raw(Total::Num))]),
            Builtin::Print => Total::Fun(vec![(Node::new_raw(Total::Num), Node::new_raw(Total::Lit(Literal::Nil)))]),
        }
    }

    pub fn eval_tuple(self, a: &BTotal, b: &BTotal, t: &BTotal) -> Result<Total, MatchError> {
        match (&**a, &**b) {
            (Total::Lit(Literal::Int(i)), Total::Lit(Literal::Int(i2))) => match self {
                Builtin::Add => Ok(Total::Lit(Literal::Int(i + i2))),
                Builtin::Mul => Ok(Total::Lit(Literal::Int(i * i2))),
                Builtin::Sub => Ok(Total::Lit(Literal::Int(i - i2))),
                Builtin::Div => Ok(Total::Lit(Literal::Int(i / i2))),
                _ => Err(MatchError::Pat(Node::new_raw(self.args()), t.clone())),
            }
            _ => Err(MatchError::Pat(Node::new_raw(self.args()), t.clone())),
        }
    }

    pub fn eval(self, x: &BTotal) -> Result<Total, MatchError> {
        match (self, &**x) {
            (Builtin::Sqr, Total::Lit(Literal::Int(i))) => Ok(Total::Lit(Literal::Int(i*i))),
            (_, Total::Tuple(a, b)) => self.eval_tuple(a, b, x),
            _ => Err(MatchError::Pat(Node::new_raw(self.args()), x.clone())),
        }
    }
}

pub fn builtins() -> Env<Builtin> {
    let mut intern_w = INTERN.write().unwrap();
    let env = vec![
        ("print", Builtin::Print),
        ("sqr", Builtin::Sqr),
        ("num", Builtin::Num),
        ("+", Builtin::Add),
        ("-", Builtin::Sub),
        ("*", Builtin::Mul),
        ("/", Builtin::Div),
    ]
    .into_iter()
    .map(|(a, b)| (intern_w.get_or_intern(a), b))
    .collect();
    Env { env }
}

pub fn totals() -> Env<BTotal> {
    let v = builtins();
    Env {
        env: v.env.into_iter().map(|(k,v)| (k, Node::new_raw(Total::Builtin(v)))).collect(),
    }
}
