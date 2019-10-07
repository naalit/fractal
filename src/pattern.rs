use crate::ast::*;
use crate::common::*;

pub type BTotal = Node<Total>;

#[derive(Debug)]
pub enum MatchError {
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
            MatchError::Pat(l, r) => {
                let message = format!("Couldn't match {:?} with {:?}", r, l);
                let message_r = format!("Matching this: {:?}", r);
                let message_l = format!("With this: {:?}", l);
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
    fn total(&self, e: &mut Env<BTotal>) -> Result<BTotal, MatchError>;
}
impl HasTotal for BTerm {
    // TODO merge with verify
    fn total(&self, env: &mut Env<BTotal>) -> Result<BTotal, MatchError> {
        let Node { span, file, .. } = self;
        let t = match &**self {
            Term::Lit(l) => Ok(Total::Lit(*l)),
            Term::Dot(n, s) => match &**n {
                Term::Lit(Literal::Int(_)) | Term::Lit(Literal::Float(_)) => {
                    Ok((*env.modules[&Type::Num][s]).clone())
                }
                Term::Var(x) => match &**env.get(*x).expect("Not found") {
                    Total::Num => Ok((*env.modules[&Type::Num][s]).clone()),
                    Total::Lit(Literal::Int(_)) => Ok((*env.modules[&Type::Num][s]).clone()),
                    Total::Lit(Literal::Float(_)) => Ok((*env.modules[&Type::Num][s]).clone()),
                    _ => Err(MatchError::MemberNotFound(n.total(env)?, Node::new(codespan::Span::new(n.span.end(), span.end()), *file, *s))),
                },
                _ => Err(MatchError::MemberNotFound(n.total(env)?, Node::new(codespan::Span::new(n.span.end(), span.end()), *file, *s))),
            },
            Term::Tuple(x, y) => {
                let (x, y) = and(x.total(env), y.total(env))?;
                Ok(Total::Tuple(x, y))
            },
            Term::Var(s) => {
                if let Some(t) = env.get(*s) {
                    Ok((**t).clone())
                } else {
                    Ok(Total::Var(*s))
                }
            }
            Term::App(f, x) => {
                let x = x.total(env)?;
                let f = f.total(env)?;
                match &*f.val {
                    // TODO check for multiple branches that together provide exhaustiveness
                    Total::Fun(f) => {
                        let v: Vec<_> = f
                            .iter()
                            .map(|(p, b)| (p.will_match(&x), b))
                            .collect();
                        if let Some((_,body)) = v.iter().find(|(p,_)| p.is_ok()) {
                            Ok((*body.val).clone())
                        } else {
                            Err(MatchError::Multi(v.into_iter().filter(|(p,_)| p.is_err()).map(|(p,_)| p.unwrap_err()).collect()))
                        }
                    }
                    _ => Err(MatchError::NotFun(f)),
                }
            }
            Term::Union(x, y) => {
                let (x, y) = and(x.total(env), y.total(env))?;
                Ok(Total::Union(x, y))
            },
            Term::Fun(v) => {
                let (ok, err): (Vec<_>, Vec<_>) = v.iter()
                        .map(|Fun { lhs, rhs }| {
                            let l = lhs.total(env)?;
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
                            let r = rhs.total(env)?;
                            // Put the old env definitions back
                            for k in remove {
                                env.env.remove(&k);
                            }
                            env.env.extend(old);
                            Ok((l, r))
                        }).partition(|x| x.is_ok());
                if err.is_empty() {
                    Ok(Total::Fun(ok.into_iter().map(|x| x.unwrap()).collect()))
                } else {
                    Err(MatchError::Multi(err.into_iter().map(|x| x.unwrap_err()).collect()))
                }
            },
            Term::Inter(x, y) => {
                let (x, y) = and(x.total(env), y.total(env))?;
                Ok(Total::Inter(x, y))
            },
            Term::Def(lhs, rhs) => {
                let (l, r) = and(lhs.total(env), rhs.total(env))?;
                match l.will_match(&r) {
                    Ok(v) => {
                        env.env.extend(v);
                        Ok(Total::Lit(Literal::Nil))
                    },
                    Err(e) => Err(e)
                }
            }
            _ => Err(MatchError::Multi(vec![])),
        }?;
        Ok(Node::new(*span, *file, t))
    }
}

impl BTotal {
    fn free_vars(&self) -> Vec<Node<Sym>> {
        match &**self {
            Total::Var(s) => vec![Node::new(self.span, self.file, *s)],
            Total::Inter(a, b) | Total::Union(a, b) | Total::Tuple(a, b) => merge(a.free_vars(), b.free_vars()),
            Total::Fun(v) => v.iter().map(|(_,b)| b.free_vars()).flatten().collect(),
            _ => vec![],
        }
    }
}

pub fn verify(n: &BTerm, env: &mut Env<BTotal>) -> Result<(), MatchError> {
    match n.total(env) {
        Ok(x) => {
            let fv = x.free_vars();
            if fv.is_empty() {
                Ok(())
            } else {
                Err(MatchError::Multi(fv.into_iter().map(MatchError::NotFound).collect()))
            }
        },
        Err(e) => Err(e),
    }
}

/// The set of possible patterns that a value can take
/// Mostly a subset of terms
/// It's possible to pattern match exactly using this, but not evaluate a function body
#[derive(Debug, PartialEq, Clone)]
pub enum Total {
    Fun(Vec<(BTotal, BTotal)>),
    Union(BTotal, BTotal),
    Inter(BTotal, BTotal),
    Lit(Literal),
    Tuple(BTotal, BTotal),
    /// Recursive definitions use essentially nominal typing, where each definition is a unique type
    Rec(usize),
    /// Var() means that this wasn't bound when we created the Total, so it's on the pattern side and needs to be bound or it's invalid
    Var(Sym),
    Num,
    Any,
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
    /// The totals that each variable is guaranteed to have if the pattern matches
    /// E.g. 'x:3' -> [(Sym(x), Total::Unit(Value::Int(3)))]
    pub fn guaranteed(&self) -> Vec<(Sym, BTotal)> {
        println!("Guaranteed for {:?}", self);
        let Node { span, file, .. } = self;
        match &**self {
            Total::Var(x) => vec![(*x, Node::new(*span, *file, Total::Any))],
            Total::Tuple(a, b) => merge(a.guaranteed(), b.guaranteed()),
            Total::Inter(a, b) => {
                // HACK
                let mut bg = b.guaranteed();
                match &**a {
                    Total::Var(s) => {
                        bg.push((*s, b.clone()));
                        bg
                    }
                    _ => bg,
                }
            }
            // TODO scenarios like 'A x | B x'
            _ => vec![],
        }
    }

    pub fn will_match(&self, other: &BTotal) -> WillMatch {
        match (&**self, &**other) {
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
            _ => Err(MatchError::Pat(self.clone(), other.clone())),
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum MatchResult {
    Pass(Vec<(Sym, Value)>),
    Fail,
}
impl MatchResult {
    pub fn is_pass(&self) -> bool {
        match self {
            MatchResult::Pass(_) => true,
            MatchResult::Fail => false,
        }
    }
    pub fn and(self, other: Self) -> Self {
        match (self, other) {
            (MatchResult::Pass(mut a), MatchResult::Pass(mut b)) => {
                a.append(&mut b);
                MatchResult::Pass(a)
            }
            _ => MatchResult::Fail,
        }
    }
}

pub trait Match {
    fn match_strict(&self, other: &Value, env: &Env<impl Match>) -> MatchResult;
}

// https://randomascii.wordpress.com/2012/02/25/comparing-floating-point-numbers-2012-edition/
// We want pattern matching on floats to work as expected, which means work modulo rounding errors
// Might be too slow to occur in generated code, especially because this might be to e.g. not divide by zero, where == works fine
fn roughly_equal_f32(a: f32, b: f32) -> bool {
    let diff = (a - b).abs();
    let largest_abs = a.abs().max(b.abs());
    // Check for absolute epsilon (needed for denormals & zero) and then relative epsilon
    // We could use ULPs, but that's more annoying and could be slower on some architectures
    diff <= std::f32::EPSILON || diff <= largest_abs * std::f32::EPSILON
}

impl Match for Term {
    fn match_strict(&self, other: &Value, env: &Env<impl Match>) -> MatchResult {
        match (self, other) {
            (Term::Lit(l), Value::Lit(l2)) if l == l2 => MatchResult::Pass(Vec::new()),
            (Term::Var(s), x) if !env.env.contains_key(s) => {
                MatchResult::Pass(vec![(*s, x.clone())])
            }
            (Term::Tuple(a, b), Value::Tuple(a2, b2)) => {
                if let MatchResult::Pass(mut a) = a.match_strict(a2, env) {
                    if let MatchResult::Pass(mut b) = b.match_strict(b2, env) {
                        a.append(&mut b);
                        return MatchResult::Pass(a);
                    }
                }
                MatchResult::Fail
            }
            (Term::Union(a, b), x) => {
                let a = a.match_strict(x, env);
                if a.is_pass() {
                    a
                } else {
                    b.match_strict(x, env)
                }
            }
            (Term::Inter(a, b), x) => a.match_strict(x, env).and(b.match_strict(x, env)),
            _ => MatchResult::Fail,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fresh_var() -> Sym {
        use string_interner::StringInterner;
        StringInterner::new().get_or_intern("a")
    }
    fn fresh_env() -> crate::ast::Env<Value> {
        crate::ast::Env {
            env: std::collections::HashMap::new(),
            modules: std::collections::HashMap::new(),
        }
    }

    #[test]
    fn test_var() {
        let env = fresh_env();

        assert!(Node::new_raw(Total::Var(fresh_var()))
            .will_match(&Node::new_raw(Total::Lit(Literal::Int(3))))
            .is_ok(),);
        assert_eq!(
            Term::Var(fresh_var()).match_strict(&Value::Lit(Literal::Int(3)), &env),
            MatchResult::Pass(vec![(fresh_var(), Value::Lit(Literal::Int(3)))])
        );
    }

    #[test]
    fn test_tuple() {
        let env = fresh_env();

        let i = Node::new_raw(Term::Lit(Literal::Int(2)));
        let j = Node::new_raw(Term::Lit(Literal::Int(3)));

        let iv = Box::new(Value::Lit(Literal::Int(2)));
        let jv = Box::new(Value::Lit(Literal::Int(3)));
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

        assert_eq!(
            Term::Tuple(i.clone(), j.clone())
                .match_strict(&Value::Tuple(iv.clone(), jv.clone()), &env),
            MatchResult::Pass(vec![])
        );
        assert_eq!(
            Term::Tuple(i, j).match_strict(&Value::Tuple(jv, iv), &env),
            MatchResult::Fail
        );
    }

    #[test]
    fn test_union() {
        let env = fresh_env();

        let i = Node::new_raw(Term::Lit(Literal::Int(2)));
        let j = Node::new_raw(Term::Lit(Literal::Int(3)));
        let v = Node::new_raw(Term::Var(fresh_var()));

        let iv = Box::new(Value::Lit(Literal::Int(2)));
        let jv = Box::new(Value::Lit(Literal::Int(3)));
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

        assert_eq!(
            Term::Union(i.clone(), j.clone()).match_strict(&Value::Lit(Literal::Int(3)), &env),
            MatchResult::Pass(vec![])
        );
        assert_eq!(
            Term::Union(i.clone(), v.clone()).match_strict(&Value::Lit(Literal::Int(3)), &env),
            MatchResult::Pass(vec![(fresh_var(), Value::Lit(Literal::Int(3)))])
        );
        assert_eq!(
            Term::Union(i, j).match_strict(&Value::Tuple(jv, iv), &env),
            MatchResult::Fail
        );
    }
}
