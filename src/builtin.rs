use crate::ast::Env;
use crate::common::*;
use crate::pattern::{BTotal, MatchError, Total};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Builtin {
    Print,
    Sqr,
    Add,
    Sub,
    Mul,
    Div,
    Num,
}
impl Builtin {
    pub fn args(self) -> Total {
        match self {
            Builtin::Num => Total::Void,
            Builtin::Sqr => Total::Num,
            Builtin::Sub | Builtin::Add | Builtin::Mul | Builtin::Div => {
                Total::Tuple(Node::new_raw(Total::Num), Node::new_raw(Total::Num))
            }
            Builtin::Print => Total::Num,
        }
    }

    pub fn ret(self) -> Total {
        match self {
            Builtin::Num => Total::Void,
            Builtin::Print => Total::Lit(Literal::Nil),
            Builtin::Sub | Builtin::Add | Builtin::Mul | Builtin::Div | Builtin::Sqr => Total::Num,
        }
    }

    pub fn pat(self) -> Total {
        match self {
            Builtin::Num => Total::Num,
            // You can't match most builtins
            _ => Total::Void,
        }
    }

    pub fn total(self) -> Total {
        match self {
            Builtin::Sub | Builtin::Add | Builtin::Mul | Builtin::Div | Builtin::Sqr => {
                Total::Fun(vec![(Node::new_raw(Total::Num), Node::new_raw(Total::Num))])
            }
            Builtin::Print => Total::Fun(vec![(
                Node::new_raw(Total::Num),
                Node::new_raw(Total::Lit(Literal::Nil)),
            )]),
            Builtin::Num => Total::Void,
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
            },
            (Total::Lit(Literal::Float(i)), Total::Lit(Literal::Float(i2))) => match self {
                Builtin::Add => Ok(Total::Lit(Literal::Float(i + i2))),
                Builtin::Mul => Ok(Total::Lit(Literal::Float(i * i2))),
                Builtin::Sub => Ok(Total::Lit(Literal::Float(i - i2))),
                Builtin::Div => Ok(Total::Lit(Literal::Float(i / i2))),
                _ => Err(MatchError::Pat(Node::new_raw(self.args()), t.clone())),
            },
            (Total::Defined(_, x), _) => self.eval_tuple(x, b, t),
            (_, Total::Defined(_, x)) => self.eval_tuple(a, x, t),
            _ => Err(MatchError::Pat(Node::new_raw(self.args()), t.clone())),
        }
    }

    pub fn eval(self, x: &BTotal) -> Result<Total, MatchError> {
        if let Ok(x) = match (self, &**x) {
            (Builtin::Sqr, Total::Lit(Literal::Int(i))) => Ok(Total::Lit(Literal::Int(i * i))),
            (Builtin::Sqr, Total::Lit(Literal::Float(i))) => Ok(Total::Lit(Literal::Float(i * i))),
            (_, Total::Tuple(a, b)) => self.eval_tuple(a, b, x),
            (_, Total::Defined(_, t)) => self.eval(t),
            (Builtin::Print, Total::Lit(i)) => {
                println!("{}", i);
                Ok(Total::Lit(Literal::Nil))
            }
            _ => Err(MatchError::Pat(Node::new_raw(self.args()), x.clone())),
        } {
            Ok(x)
        } else {
            match Node::new_raw(self.args()).will_match(x) {
                Ok(_) => Ok(self.ret()),
                Err(e) => Err(e),
            }
        }
    }
}

pub fn builtins() -> Env<Builtin> {
    let mut intern_w = INTERN.write().unwrap();
    let env = vec![
        ("print", Builtin::Print),
        ("sqr", Builtin::Sqr),
        ("+", Builtin::Add),
        ("-", Builtin::Sub),
        ("*", Builtin::Mul),
        ("/", Builtin::Div),
        ("num", Builtin::Num),
    ]
    .into_iter()
    .map(|(a, b)| (intern_w.get_or_intern(a), b))
    .collect();
    Env { env }
}

pub fn totals() -> Env<BTotal> {
    let v = builtins();
    Env {
        env: v
            .env
            .into_iter()
            .map(|(k, v)| (k, Node::new_raw(Total::Builtin(v))))
            // .chain(vec![(
            //     INTERN.write().unwrap().get_or_intern("num"),
            //     Node::new_raw(Total::Num),
            // )])
            .collect(),
    }
}
