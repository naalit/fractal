use crate::ast::{Env, Type};
use crate::common::*;
use crate::pattern::{BTotal, Total};

#[derive(Clone, Debug, PartialEq)]
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

pub fn values() -> Env<Value> {
    let mut intern_w = INTERN.write().unwrap();
    let env = vec![
        ("print", Builtin::Print),
        ("sqr", Builtin::Sqr),
        ("num", Builtin::Num),
    ]
    .into_iter()
    .map(|(a, b)| (intern_w.get_or_intern(a), Value::Builtin(b)))
    .collect();
    let modules = vec![(
        Type::Num,
        vec![
            ("+", Builtin::Add),
            ("-", Builtin::Sub),
            ("*", Builtin::Mul),
            ("/", Builtin::Div),
        ]
        .into_iter()
        .map(|(a, b)| (intern_w.get_or_intern(a), Value::Builtin(b)))
        .collect(),
    )]
    .into_iter()
    .collect();
    Env { env, modules }
}

pub fn totals() -> Env<BTotal> {
    let mut intern_w = INTERN.write().unwrap();
    let tfun = Node::new_raw(Total::Fun(vec![(
        Node::new_raw(Total::Num),
        Node::new_raw(Total::Num),
    )]));
    let t_nu = Node::new_raw(Total::Fun(vec![(
        Node::new_raw(Total::Num),
        Node::new_raw(Total::Lit(Literal::Nil)),
    )]));
    let env = vec![
        ("print", t_nu.clone()),
        ("sqr", tfun.clone()),
        ("num", Node::new_raw(Total::Num)),
    ]
    .into_iter()
    .map(|(a, b)| (intern_w.get_or_intern(a), b))
    .collect();
    let modules = vec![(
        Type::Num,
        vec![
            ("+", tfun.clone()),
            ("-", tfun.clone()),
            ("*", tfun.clone()),
            ("/", tfun.clone()),
        ]
        .into_iter()
        .map(|(a, b)| (intern_w.get_or_intern(a), b))
        .collect(),
    )]
    .into_iter()
    .collect();
    Env { env, modules }
}
