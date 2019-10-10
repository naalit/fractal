use crate::builtin::Builtin;
use crate::ast::*;
use crate::common::*;

pub type BTotal = Node<Total>;

#[derive(Debug)]
pub enum MatchError {
    Custom(Node<String>),
    Pat(BTotal, BTotal),
    NotFun(BTotal),
    NotFound(Node<Sym>),
    MemberNotFound(BTotal, Node<Sym>),
    Multi(Vec<MatchError>),
}
impl MatchError {
    pub fn error(self) -> Vec<crate::error::Error> {
        use crate::error::*;
        match self {
            MatchError::Custom(s) => {
                vec![Error::new(s.file, &*s, s.span, "")]
            }
            MatchError::Pat(l, r) => {
                let message = format!("Couldn't match '{}' with '{}'", r, l);
                let message_r = format!("Matching this: '{}'", r);
                let message_l = format!("With this: '{}'", l);
                vec![
                    Error::new(r.file, &message, r.span, message_r),
                    Error::new(l.file, message, l.span, message_l),
                ]
            }
            MatchError::Multi(v) => v.into_iter().flat_map(|x| x.error()).collect(),
            MatchError::NotFun(f) => vec![Error::new(
                f.file,
                format!("{:?} is not a function", f),
                f.span,
                "Used here as a function",
            )],
            MatchError::MemberNotFound(n, sym) => {
                let s = INTERN.read().unwrap().resolve(*sym).unwrap().to_owned();
                let message = format!("Member {} not found for {:?}", s, n);
                let message_2 = format!("Member not found: Note: this has guaranteed type {:?}", n);
                let message_l = format!("No member {} on this", s);
                let message_r = "Member not found".to_string();
                vec![
                    Error::new(n.file, message_2, n.span, message_l),
                    Error::new(sym.file, message, sym.span, message_r),
                ]
            }
            MatchError::NotFound(s) => vec![Error::new(
                s.file,
                format!("Not found: {}", INTERN.read().unwrap().resolve(*s).unwrap()),
                s.span,
                "Not found",
            )],
        }
    }
}

pub fn and<T>(x: Result<T, MatchError>, y: Result<T, MatchError>) -> Result<(T, T), MatchError> {
    match (x, y) {
        (Ok(x), Ok(y)) => Ok((x, y)),
        (Err(e), Err(e2)) => Err(MatchError::Multi(vec![e, e2])),
        (Err(e), _) => Err(e),
        (_, Err(e)) => Err(e),
    }
}

pub trait HasTotal {
    fn total(&self, e: &mut Env<BTotal>, pat: bool) -> Result<BTotal, MatchError>;
}

pub trait TotalCompat: Sized {
    fn to_total(&self) -> Option<BTotal>;
    fn from_total(t: &BTotal) -> Option<Self>;
}

impl TotalCompat for BTotal {
    fn to_total(&self) -> Option<BTotal> {
        Some(self.clone())
    }
    fn from_total(t: &BTotal) -> Option<BTotal> {
        t.to_total()
    }
}

/// The set of possible patterns that a value can take
/// This is the IR that we use for partial evaluation and validation
/// So it can be used for general evaluation as well
#[derive(Debug, PartialEq, Clone)]
pub enum Total {
    App(BTotal, BTotal),
    Fun(Vec<(BTotal, BTotal)>),
    Builtin(Builtin),
    /// It could be either of these things
    Or(BTotal, BTotal),
    /// This is a written union, x|y. It's not used for 'either of these things' for totals
    Union(BTotal, BTotal),
    Inter(BTotal, BTotal),
    Lit(Literal),
    Tuple(BTotal, BTotal),
    /// Recursive definitions use essentially nominal typing, where each definition is a unique type
    Rec(usize),
    /// Var() means that this wasn't bound when we created the Total, so it's on the pattern side and needs to be bound or it's invalid
    Var(Sym),
    /// A variable with a known pattern that can be refined
    Defined(Sym, BTotal),
    /// It could be any number. If the user writes 'num', that's Builtin(Builtin::Num), which evaluates to this only on the pattern side
    Num,
    /// Nothing inhabits this, and it matches nothing
    Void,
    Any,
}

impl std::fmt::Display for BTotal {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &**self {
            Total::Void => write!(f, "!"),
            Total::Any => write!(f, "_"),
            Total::Num => write!(f, "num"),
            Total::Defined(s, t) => write!(f, "{}: {}", INTERN.read().unwrap().resolve(*s).unwrap(), t),
            Total::Var(s) => write!(f, "{}", INTERN.read().unwrap().resolve(*s).unwrap()),
            Total::Rec(u) => write!(f, "<deprecated Rec {}>", u),
            Total::App(a, b) => write!(f, "({})({})", a, b),
            Total::Tuple(a, b) => write!(f, "{}, {}", a, b),
            Total::Inter(a, b) => write!(f, "{} : {}", a, b),
            Total::Lit(l) => write!(f, "{}", l),
            Total::Union(a, b) => write!(f, "{} | {}", a, b),
            // TODO: print this differently from unions
            Total::Or(a, b) => write!(f, "{} | {}", a, b),
            Total::Fun(v) => {
                write!(f, "fun")?;
                for i in v {
                    write!(f, "\n  {} => {}", i.0, i.1)?;
                }
                writeln!(f)
            },
            Total::Builtin(b) => write!(f, "{:?}", b),
        }
    }
}

pub type WillMatch = Result<Vec<(Sym, BTotal)>, MatchError>;

pub fn both(a: WillMatch, b: WillMatch) -> WillMatch {
    match (a, b) {
        (Ok(x), Ok(y)) => Ok(merge(x, y)),
        (Err(e), Err(e2)) => Err(MatchError::Multi(vec![e, e2])),
        (Err(e), _) => Err(e),
        (_, Err(e)) => Err(e),
    }
}

pub fn merge<T>(mut a: Vec<T>, mut b: Vec<T>) -> Vec<T> {
    a.append(&mut b);
    a
}

impl BTotal {
    /// 'Simple' terms are ones that we let you return from functions (and remove the App node)
    fn simple(&self) -> bool {
        match &**self {
            Total::Lit(_) | Total::Builtin(_) => true,
            Total::Tuple(a, b) => a.simple() && b.simple(),
            _ => false,
        }
    }

    /// What this matches
    fn pat(&self) -> BTotal {
        match &**self {
            Total::Var(_) => self.replace(Total::Any),
            Total::Tuple(a, b) => self.replace(Total::Tuple(a.pat(), b.pat())),
            Total::Union(a, b) => self.replace(Total::Or(a.pat(), b.pat())),
            Total::Inter(a, b) => self.replace(Total::Inter(a.pat(), b.pat())),
            Total::App(_, _) | Total::Rec(_) | Total::Num | Total::Or(_, _) => self.replace(Total::Void),
            Total::Defined(_, x) => x.pat(),
            Total::Builtin(b) => self.replace(b.pat()),
            // TODO
            Total::Fun(_) => self.replace(Total::Any),
            Total::Any | Total::Void | Total::Lit(_) => self.clone(),
        }
    }

    /// The totals that each variable is guaranteed to have if the pattern matches
    /// E.g. 'x:3' -> [(Sym(x), Total::Unit(Value::Int(3)))]
    pub fn guaranteed(&self) -> Vec<(Sym, BTotal)> {
        println!("Guaranteed for {:?}", self);
        match &**self {
            Total::Var(x) => vec![(*x, self.replace(Total::Any))],
            Total::Tuple(a, b) => merge(a.guaranteed(), b.guaranteed()),
            Total::Inter(a, b) => {
                // HACK
                let mut bg = b.guaranteed();
                match &**a {
                    Total::Var(s) => {
                        println!("B is {}", b);
                        bg.push((*s, b.pat()));
                        bg
                    }
                    _ => bg,
                }
            }
            // TODO scenarios like 'A x | B x'
            _ => vec![],
        }
    }

    pub fn simplify_mut(&mut self, env: &mut Env<BTotal>) {
        if let Some(a) = self.simplify(env) {
            *self = a;
        };
    }

    pub fn move_env_mut(&mut self) {
        if let Some(a) = self.move_env() {
            *self = a;
        }
    }

    pub fn app(self, other: BTotal, env: &mut Env<BTotal>) -> Result<BTotal, MatchError> {
        let a = match &*self {
            Total::Builtin(b) => b.eval(&other).map(|x| self.replace(x))?,
            // TODO check for multiple branches that together provide exhaustiveness
            Total::Fun(f) => {
                let v: Vec<_> = f.iter().map(|(p, b)| (p.will_match(&other), b)).collect();
                if let Some((sub, body)) = v.iter().find(|(p, _)| p.is_ok()) {
                    let mut body = (*body).clone();
                    let sub = sub.as_ref().unwrap().clone();

                    let mut rem = Vec::new();
                    let mut old = Vec::new();

                    for (k,v) in sub {
                        rem.push(k);
                        if let Some(v) = env.env.insert(k, v) {
                            old.push((k,v));
                        }
                    }

                    body.simplify_mut(env);
                    body.move_env_mut();

                    for k in rem {
                        env.env.remove(&k);
                    }
                    for (k,v) in old {
                        env.env.insert(k, v);
                    }

                    Ok(body)
                } else {
                    Err(MatchError::Multi(
                        v.into_iter()
                            .filter(|(p, _)| p.is_err())
                            .map(|(p, _)| p.unwrap_err())
                            .collect(),
                    ))
                }
            }?,
            _ => return Err(MatchError::NotFun(self)),
        };
        if a.simple() {
            Ok(a)
        } else {
            Ok(Node::new(codespan::Span::new(self.span.start(), other.span.end()), self.file, Total::App(self, other)))
        }
    }

    pub fn move_env(&self) -> Option<BTotal> {
        let multi =
            |cons: fn(BTotal, BTotal) -> Total, a: &BTotal, b: &BTotal| match a.move_env() {
                Some(a) => match b.move_env() {
                    Some(b) => Some(cons(a, b)),
                    None => Some(cons(a, b.clone())),
                },
                None => match b.move_env() {
                    Some(b) => Some(cons(a.clone(), b)),
                    None => None,
                },
            };

        let Node { span, file, .. } = self;
        let (span, file) = (*span, *file);

        let n = match &*self.val {
            Total::Var(_) => Some(Total::Any),
            Total::Defined(_, t) => return Some(t.clone()),
            Total::Inter(a, b) => multi(Total::Inter, a, b),
            Total::Tuple(a, b) => multi(Total::Tuple, a, b),
            Total::Union(a, b) => multi(Total::Union, a, b),
            // TODO fun
            _ => None,
        };

        n.map(|x| Node::new(span, file, x))
    }

    pub fn simplify(&self, env: &mut Env<BTotal>) -> Option<BTotal> {
        let mut multi =
            |cons: fn(BTotal, BTotal) -> Total, a: &BTotal, b: &BTotal| match a.simplify(env) {
                Some(a) => match b.simplify(env) {
                    Some(b) => Some(self.replace(cons(a, b))),
                    None => Some(self.replace(cons(a, b.clone()))),
                },
                None => match b.simplify(env) {
                    Some(b) => Some(self.replace(cons(a.clone(), b))),
                    None => None,
                },
            };

        match &*self.val {
            Total::Defined(s, t) => match env.get(*s) {
                Some(u) => {
                    assert!(t == u || t.will_match(u).is_ok(), "Total::Defined was narrowed to {}, which doesn't match the previous value of {}", u, t);
                    Some(self.replace(Total::Defined(*s, u.clone())))
                }
                None => None,
            },
            Total::Inter(a, b) => multi(Total::Inter, a, b),
            Total::Tuple(a, b) => multi(Total::Tuple, a, b),
            Total::Union(a, b) => multi(Total::Union, a, b),
            Total::App(a, b) => {
                let a = a.simplify(env).unwrap_or_else(|| a.clone());
                let b = b.simplify(env).unwrap_or_else(|| b.clone());
                let app = a.app(b, env).expect("Could not apply a and b in simplify()!");
                Some(app)
            }
            // TODO Fun
            _ => None,
        }
    }

    pub fn will_match(&self, other: &BTotal) -> WillMatch {
        match (&**self, &**other) {
            (Total::Builtin(Builtin::Num), _) => self.replace(Total::Num).will_match(other),
            (_, Total::Builtin(x)) => self.will_match(&other.replace(x.total())),
            (Total::Num, Total::Num) => Ok(vec![]),
            (Total::Num, Total::Lit(Literal::Int(_))) => Ok(vec![]),
            (Total::Num, Total::Lit(Literal::Float(_))) => Ok(vec![]),
            (Total::Var(s), _) => Ok(vec![(*s, other.clone())]),
            (Total::Any, _) => Ok(vec![]),
            (Total::Lit(a), Total::Lit(b)) => {
                if a == b {
                    Ok(vec![])
                } else {
                    Err(MatchError::Pat(self.clone(), other.clone()))
                }
            }
            (Total::Tuple(a, b), Total::Tuple(a2, b2)) => both(a.will_match(a2), b.will_match(b2)),
            (Total::Inter(a, b), _) => both(a.will_match(other), b.will_match(other)),
            (Total::Union(a, b), _) => match a.will_match(other) {
                // Unions don't bind variables
                Ok(_) => Ok(vec![]),
                Err(e) => match b.will_match(other) {
                    Ok(_) => Ok(vec![]),
                    Err(e2) => Err(MatchError::Multi(vec![e, e2])),
                },
            },
            (Total::Defined(_, t), _) => t.will_match(other),
            (_, Total::Defined(_, t)) => self.will_match(t),
            _ => Err(MatchError::Pat(self.clone(), other.clone())),
        }
    }
}
impl HasTotal for BTerm {
    // TODO merge with verify
    fn total(&self, env: &mut Env<BTotal>, pat: bool) -> Result<BTotal, MatchError> {
        let Node { span, file, .. } = self;
        let t = match &**self {
            Term::Lit(l) => Ok(Total::Lit(*l)),
            Term::Dot(n, s) => Err(MatchError::MemberNotFound(
                        n.total(env, pat)?,
                        Node::new(codespan::Span::new(n.span.end(), span.end()), *file, *s),
                    )),
            Term::Tuple(x, y) => {
                let (x, y) = and(x.total(env, pat), y.total(env, pat))?;
                Ok(Total::Tuple(x, y))
            }
            Term::Var(s) => {
                if let Some(t) = env.get(*s) {
                    Ok(Total::Defined(*s, t.clone()))
                } else if pat {
                    Ok(Total::Var(*s))
                } else {
                    Err(MatchError::NotFound(Node::new(*span, *file, *s)))
                }
            }
            Term::App(f, x) => {
                let x = x.total(env, pat)?;
                let mut f = f.total(env, pat)?;
                if let Total::Defined(_, x) = &*f {
                    f = x.clone();
                }
                f.app(x, env).map(|x| (*x).clone())
            }
            Term::Union(x, y) => {
                let (x, y) = and(x.total(env, pat), y.total(env, pat))?;
                Ok(Total::Union(x, y))
            }
            Term::Fun(v) => {
                let (ok, err): (Vec<_>, Vec<_>) = v
                    .iter()
                    .map(|Fun { lhs, rhs }| {
                        let l = lhs.total(env, true)?;
                        let g = l.guaranteed();
                        // Keep track of what we're adding to the env
                        let mut old = Vec::new();
                        let mut remove = Vec::with_capacity(g.len());
                        for (k, v) in g {
                            remove.push(k);
                            if let Some(v) = env.env.insert(k, v) {
                                old.push((k, v));
                            }
                        }
                        let r = rhs.total(env, pat)?;
                        // Put the old env definitions back
                        for k in remove {
                            env.env.remove(&k);
                        }
                        env.env.extend(old);
                        Ok((l, r))
                    })
                    .partition(|x| x.is_ok());
                if err.is_empty() {
                    Ok(Total::Fun(ok.into_iter().map(|x| x.unwrap()).collect()))
                } else {
                    Err(MatchError::Multi(
                        err.into_iter().map(|x| x.unwrap_err()).collect(),
                    ))
                }
            }
            Term::Inter(x, y) => {
                if pat {
                    let (x, y) = and(x.total(env, pat), y.total(env, pat))?;
                    Ok(Total::Inter(x, y))
                } else {
                    // We don't want the user to be able to create random intersections and pass them around
                    Err(MatchError::Custom(self.replace(String::from("Intersections are only allowed on the pattern side"))))
                }
            }
            Term::Def(lhs, rhs) => {
                let (l, r) = and(lhs.total(env, true), rhs.total(env, pat))?;
                match l.will_match(&r) {
                    Ok(v) => {
                        env.env.extend(v);
                        Ok(Total::Lit(Literal::Nil))
                    }
                    Err(e) => Err(e),
                }
            }
            Term::Block(v) => {
                let mut t = Node::new(*span, *file, Total::Lit(Literal::Nil));
                for i in v {
                    t = i.total(env, pat)?;
                }
                return Ok(t);
            }
            // TODO remove rec
            Term::Rec(_) => Ok(Total::Any),
        }?;
        Ok(Node::new(*span, *file, t))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fresh_var() -> Sym {
        use string_interner::StringInterner;
        StringInterner::new().get_or_intern("a")
    }
    fn fresh_env() -> crate::ast::Env<Term> {
        crate::ast::Env {
            env: std::collections::HashMap::new(),
        }
    }

    #[test]
    fn test_var() {
        let env = fresh_env();

        assert!(Node::new_raw(Total::Var(fresh_var()))
            .will_match(&Node::new_raw(Total::Lit(Literal::Int(3))))
            .is_ok(),);
        // assert_eq!(
        //     Term::Var(fresh_var()).match_strict(&Value::Lit(Literal::Int(3)), &env),
        //     MatchResult::Pass(vec![(fresh_var(), Value::Lit(Literal::Int(3)))])
        // );
    }

    #[test]
    fn test_tuple() {
        let env = fresh_env();

        let i = Node::new_raw(Term::Lit(Literal::Int(2)));
        let j = Node::new_raw(Term::Lit(Literal::Int(3)));

        // let iv = Box::new(Value::Lit(Literal::Int(2)));
        // let jv = Box::new(Value::Lit(Literal::Int(3)));
        let ir = Node::new_raw(Total::Lit(Literal::Int(2)));
        let jr = Node::new_raw(Total::Lit(Literal::Int(3)));
        let vr = Node::new_raw(Total::Any);

        assert!(Node::new_raw(Total::Tuple(ir.clone(), jr.clone()))
            .will_match(&Node::new_raw(Total::Tuple(ir.clone(), jr.clone())))
            .is_ok());
        assert!(Node::new_raw(Total::Tuple(ir.clone(), jr.clone()))
            .will_match(&Node::new_raw(Total::Tuple(ir.clone(), vr.clone())))
            .is_err());
        assert!(Node::new_raw(Total::Tuple(ir.clone(), jr.clone()))
            .will_match(&Node::new_raw(Total::Tuple(jr.clone(), vr.clone())))
            .is_err());

        // assert_eq!(
        //     Term::Tuple(i.clone(), j.clone())
        //         .match_strict(&Value::Tuple(iv.clone(), jv.clone()), &env),
        //     MatchResult::Pass(vec![])
        // );
        // assert_eq!(
        //     Term::Tuple(i, j).match_strict(&Value::Tuple(jv, iv), &env),
        //     MatchResult::Fail
        // );
    }

    #[test]
    fn test_union() {
        let env = fresh_env();

        let i = Node::new_raw(Term::Lit(Literal::Int(2)));
        let j = Node::new_raw(Term::Lit(Literal::Int(3)));
        let v = Node::new_raw(Term::Var(fresh_var()));

        // let iv = Box::new(Value::Lit(Literal::Int(2)));
        // let jv = Box::new(Value::Lit(Literal::Int(3)));
        let ir = Node::new_raw(Total::Lit(Literal::Int(2)));
        let jr = Node::new_raw(Total::Lit(Literal::Int(3)));
        let vr = Node::new_raw(Total::Any);

        assert!(Node::new_raw(Total::Union(ir.clone(), jr.clone()))
            .will_match(&ir)
            .is_ok(),);
        assert!(Node::new_raw(Total::Union(ir.clone(), jr.clone()))
            .will_match(&vr)
            .is_err());
        assert!(Node::new_raw(Total::Union(ir.clone(), vr.clone()))
            .will_match(&Node::new_raw(Total::Tuple(jr.clone(), vr.clone())))
            .is_ok());

        // assert_eq!(
        //     Term::Union(i.clone(), j.clone()).match_strict(&Value::Lit(Literal::Int(3)), &env),
        //     MatchResult::Pass(vec![])
        // );
        // assert_eq!(
        //     Term::Union(i.clone(), v.clone()).match_strict(&Value::Lit(Literal::Int(3)), &env),
        //     MatchResult::Pass(vec![(fresh_var(), Value::Lit(Literal::Int(3)))])
        // );
        // assert_eq!(
        //     Term::Union(i, j).match_strict(&Value::Tuple(jv, iv), &env),
        //     MatchResult::Fail
        // );
    }
}
