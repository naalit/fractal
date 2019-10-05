use crate::ast::*;
use crate::vm::Value;
use string_interner::Sym;
use std::rc::Rc;
use std::sync::RwLock;
use lazy_static::lazy_static;

pub type BTotal = Node<Total>;

lazy_static! {
    // We're using an RwLock and not a RefCell because static forces it to be Sync
    static ref DEF: RwLock<usize> = RwLock::new(0);
}

#[derive(Debug)]
pub enum MatchError {
    Pat(BTotal, BTotal),
    NotFun(BTotal),
    NotFound(Node<Sym>),
    Multi(Vec<MatchError>),
}
impl MatchError {
    pub fn error(self) -> Vec<crate::error::Error> {
        use crate::error::*;
        match self {
            MatchError::Pat(l,r) => {
                let message = format!("Couldn't match {:?} with {:?}", r, l);
                vec![
                Error::new(r.file, &message, r.span, "Matching this"),
                Error::new(l.file, message, l.span, "With this")]
            },
            MatchError::Multi(v) => v.into_iter().flat_map(|x| x.error()).collect(),
            MatchError::NotFun(f) => vec![Error::new(f.file, format!("{:?} is not a function", f), f.span, "Used here as a function")],
            MatchError::NotFound(s) => vec![Error::new(s.file, format!("Not found: {}", crate::parse::intern.read().unwrap().resolve(*s).unwrap()), s.span, "Not found")],
        }
    }
}

pub fn verify(n: &BTerm, env: &mut Env<BTotal>) -> Result<(), MatchError> {
    match &*n.val {
        Term::App(f, x) => {
            let x = x.total(env);
            verify(f, env)?;
            let f = f.total(env);
            match &*f.val {
                // TODO check for multiple branches that together provide exhaustiveness
                Total::Fun(f) => {
                    // TODO somehow verify pattern matches or something
                    let v: Vec<_> = f.iter().filter_map(|(p,_)| if p.will_match(&x).yes() { None } else { Some(MatchError::Pat(p.clone(), x.clone())) }).collect();
                    if v.is_empty() {
                        Ok(())
                    } else {
                        Err(MatchError::Multi(v))
                    }
                }
                _ => Err(MatchError::NotFun(f)),
            }
        }
        Term::Tuple(a,b) | Term::Inter(a,b) | Term::Union(a,b) => verify(a,env).and_then(|()| verify(b,env)),
        Term::Fun(v) => {
            // TODO scoping
            for Fun{lhs,rhs} in v {
                let g = lhs.total(env).guaranteed();
                println!("guaranteed {:?}", g);
                env.env.extend(g);
                verify(rhs, env)?;
            }
            Ok(())
        },
        Term::Block(v) => {
            // TODO scoping
            for i in v {
                verify(i, env)?
            }
            Ok(())
        }
        Term::Def(l, r) => match l.total(env).will_match(&Rc::new(r.total(env))) {
            WillMatch::Yes(v) => {
                env.env.extend(v);
                verify(r, env)
            }
            _ => Err(MatchError::Pat(l.total(env), r.total(env))),
        }
        Term::Var(v) if !env.env.contains_key(v) => Err(MatchError::NotFound(Node::new(n.span, n.file, *v))),
        _ => Ok(()),
    }
}

/// The set of possible patterns that a value can take
#[derive(Debug, PartialEq, Clone)]
pub enum Total {
    Fun(Vec<(BTotal, BTotal)>),
    Union(BTotal, BTotal),
    Inter(BTotal, BTotal),
    Unit(Value),
    Tuple(BTotal, BTotal),
    /// Recursive definitions use essentially nominal typing, where each definition is a unique type
    Rec(usize),
    /// Var() means that this wasn't bound when we created the Total, so it's on the pattern side and needs to be bound or it's invalid
    Var(Sym),
    Num,
    Any,
}

pub enum WillMatch {
    Yes(Vec<(Sym,BTotal)>),
    Maybe,
    No,
}
impl WillMatch {
    fn yes(&self) -> bool {
        match self {
            WillMatch::Yes(_) => true,
            _ => false,
        }
    }
    fn no(&self) -> bool {
        match self {
            WillMatch::No => true,
            _ => false,
        }
    }
}

pub fn merge<T>(mut a: Vec<T>, mut b: Vec<T>) -> Vec<T> {
    a.append(&mut b);
    a
}

impl BTotal {
    /// The totals that each variable is guaranteed to have if the pattern matches
    /// E.g. 'x:3' -> [(Sym(x), Total::Unit(Value::Int(3)))]
    pub fn guaranteed(&self) -> Vec<(Sym,BTotal)> {
        let Node { span, file, .. } = self;
        match &**self {
            Total::Var(x) => vec![(*x, Node::new(*span, *file, Total::Any))],
            Total::Tuple(a,b) => merge(a.guaranteed(), b.guaranteed()),
            Total::Inter(a,b) => {
                let mut bg = b.guaranteed();
                match &**a {
                    Total::Var(s) => {
                        bg.push((*s,b.clone()));
                        bg
                    },
                    _ => bg,
                }
            }
            // TODO scenarios like 'A x | B x'
            _ => vec![],
        }
    }

    pub fn will_match(&self, other: &BTotal) -> WillMatch {
        match (&**self,&**other) {
            (Total::Num, Total::Num) => WillMatch::Yes(vec![]),
            (Total::Num, Total::Unit(Value::Int(_))) => WillMatch::Yes(vec![]),
            (Total::Num, Total::Unit(Value::Float(_))) => WillMatch::Yes(vec![]),
            (Total::Var(s), _) => WillMatch::Yes(vec![(*s, other.clone())]),
            (Total::Any, _) => WillMatch::Yes(vec![]),
            (Total::Unit(a), Total::Unit(b)) => if a == b {
                WillMatch::Yes(vec![])
            } else {
                WillMatch::No
            }
            (Total::Inter(a,b), _) => match a.will_match(other) {
                WillMatch::Yes(x) => match b.will_match(other) {
                    WillMatch::Yes(y) => WillMatch::Yes(merge(x,y)),
                    n => n,
                }
                WillMatch::Maybe => match b.will_match(other) {
                    WillMatch::No => WillMatch::No,
                    _ => WillMatch::Maybe,
                }
                n => n,
            }
            (Total::Union(a,b), _) => match a.will_match(other) {
                // Unions don't bind variables
                WillMatch::Yes(_) => WillMatch::Yes(vec![]),
                _ => match b.will_match(other) {
                    WillMatch::Yes(_) => WillMatch::Yes(vec![]),
                    // This doesn't quite handle Maybes right, but that's probably fine
                    // (e.g. if a is maybe and b is no it will say no instead of maybe)
                    x => x,
                }
            }
            _ => WillMatch::Maybe,
        }
    }
}

pub trait HasTotal {
    fn total(&self, e: &Env<BTotal>) -> BTotal;
}
impl HasTotal for BTerm {
    fn total(&self, env: &Env<BTotal>) -> BTotal {
        let Node { span, file, .. } = self;
        let t = if let Some(v) = self.simple() {
            Total::Unit(v)
        } else {
            match &**self {
                Term::Dot(x,s) => match &**x {
                    Term::Int(_) | Term::Float(_) => (*env.modules[&Type::Num][s]).clone(),
                    Term::Var(x) => match &**env.get(*x).expect("Not found") {
                        Total::Num => (*env.modules[&Type::Num][s]).clone(),
                        Total::Unit(Value::Int(_)) => (*env.modules[&Type::Num][s]).clone(),
                        Total::Unit(Value::Float(_)) => (*env.modules[&Type::Num][s]).clone(),
                        _ => panic!("Old namespace system only supports numbers"),
                    }
                    _ => panic!("Old namespace system only supports numbers"),
                }
                Term::Var(s) => if let Some(t) = env.get(*s) {
                    (**t).clone()
                } else {
                    Total::Var(*s)
                }
                Term::Union(x, y) => Total::Union(x.total(env), y.total(env)),
                Term::Fun(v) => Total::Fun(v.iter().map(|x| (x.lhs.total(env), x.rhs.total(env))).collect()),
                Term::Inter(x, y) => Total::Inter(x.total(env), y.total(env)),
                _ => Total::Any,
            }
        };
        Node::new(*span, *file, t)
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
        match (self,other) {
            (MatchResult::Pass(mut a), MatchResult::Pass(mut b)) => {
                a.append(&mut b);
                MatchResult::Pass(a)
            },
            _ => MatchResult::Fail,
        }
    }
}

/// Three-value logic
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum TBool {
    True,
    False,
    Maybe,
}
impl TBool {
    pub fn is_true(self) -> bool {
        self == True
    }
    pub fn is_false(self) -> bool {
        self == True
    }
}

/// Top-level True, False, Maybe
pub use TBool::{True,False,Maybe};

impl std::ops::BitOr for TBool {
    type Output = TBool;
    fn bitor(self, rhs: TBool) -> TBool {
        match (self, rhs) {
            (True, _) => True,
            (_, True) => True,
            (False, False) => False,
            _ => Maybe,
        }
    }
}

impl std::ops::BitAnd for TBool {
    type Output = TBool;
    fn bitand(self, rhs: TBool) -> TBool {
        match (self, rhs) {
            (False, _) => False,
            (_, False) => False,
            (True, True) => True,
            _ => Maybe,
        }
    }
}

pub trait Match {
    fn match_strict(&self, other: &Value) -> MatchResult;
    fn match_conservative(&self, _other: &Total) -> TBool {
        Maybe
    }
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
    fn match_strict(&self, other: &Value) -> MatchResult {
        match (self, other) {
            (Term::Nil, Value::Nil) => MatchResult::Pass(Vec::new()),
            (Term::Var(s), x) => MatchResult::Pass(vec![(*s, x.clone())]),
            (Term::Int(x), Value::Int(y)) if x == y => MatchResult::Pass(Vec::new()),
            (Term::Float(x), Value::Float(y)) if roughly_equal_f32(*x, *y) => {
                MatchResult::Pass(Vec::new())
            }
            (Term::Tuple(a, b), Value::Tuple(a2, b2)) => {
                if let MatchResult::Pass(mut a) = a.match_strict(a2) {
                    if let MatchResult::Pass(mut b) = b.match_strict(b2) {
                        a.append(&mut b);
                        return MatchResult::Pass(a);
                    }
                }
                MatchResult::Fail
            }
            (Term::Union(a, b), x) => {
                let a = a.match_strict(x);
                if a.is_pass() {
                    a
                } else {
                    b.match_strict(x)
                }
            }
            (Term::Inter(a, b), x) => a.match_strict(x).and(b.match_strict(x)),
            _ => MatchResult::Fail,
        }
    }

    fn match_conservative(&self, other: &Total) -> TBool {
        match (self, other) {
            (_, Total::Unit(v)) => match self.match_strict(v) {
                MatchResult::Pass(_) => True,
                MatchResult::Fail => False,
            }
            (Term::Nil, _) => False,
            (Term::Var(_), _) => True,
            (_, Total::Any) => Maybe,
            (Term::Tuple(a, b), Total::Tuple(a2, b2)) => {
                a.match_conservative(a2) & b.match_conservative(b2)
            }
            (Term::Union(a, b), x) => a.match_conservative(x) | b.match_conservative(x),
            (Term::Inter(a, b), x) => a.match_conservative(x) & b.match_conservative(x),
            _ => Maybe,
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

    #[test]
    fn test_var() {
        assert_eq!(
            Term::Var(fresh_var()).match_conservative(&Total::Unit(Value::Int(3))),
            True
        );
        assert_eq!(
            Term::Var(fresh_var()).match_strict(&Value::Int(3)),
            MatchResult::Pass(vec![(fresh_var(), Value::Int(3))])
        );
    }

    #[test]
    fn test_tuple() {
        let i = Node::new_raw(Term::Int(2));
        let j = Node::new_raw(Term::Int(3));

        let iv = Box::new(Value::Int(2));
        let jv = Box::new(Value::Int(3));
        let ir = Node::new_raw(Total::Unit(Value::Int(2)));
        let jr = Node::new_raw(Total::Unit(Value::Int(3)));
        let vr = Node::new_raw(Total::Any);

        assert_eq!(
            Term::Tuple(i.clone(), j.clone())
                .match_conservative(&Total::Tuple(ir.clone(), jr.clone())),
            True
        );
        assert_eq!(
            Term::Tuple(i.clone(), j.clone())
                .match_conservative(&Total::Tuple(ir.clone(), vr.clone())),
            Maybe
        );
        assert_eq!(
            Term::Tuple(i.clone(), j.clone())
                .match_conservative(&Total::Tuple(jr.clone(), vr.clone())),
            False
        );

        assert_eq!(
            Term::Tuple(i.clone(), j.clone()).match_strict(&Value::Tuple(iv.clone(), jv.clone())),
            MatchResult::Pass(vec![])
        );
        assert_eq!(
            Term::Tuple(i, j).match_strict(&Value::Tuple(jv, iv)),
            MatchResult::Fail
        );
    }

    #[test]
    fn test_union() {
        let i = Node::new_raw(Term::Int(2));
        let j = Node::new_raw(Term::Int(3));
        let v = Node::new_raw(Term::Var(fresh_var()));

        let iv = Box::new(Value::Int(2));
        let jv = Box::new(Value::Int(3));
        let ir = Node::new_raw(Total::Unit(Value::Int(2)));
        let jr = Node::new_raw(Total::Unit(Value::Int(3)));
        let vr = Node::new_raw(Total::Any);

        assert_eq!(
            Term::Union(i.clone(), j.clone()).match_conservative(&ir),
            True
        );
        assert_eq!(
            Term::Union(i.clone(), j.clone()).match_conservative(&vr),
            Maybe
        );
        assert_eq!(
            Term::Union(i.clone(), v.clone())
                .match_conservative(&Total::Tuple(jr.clone(), vr.clone())),
            True
        );

        assert_eq!(
            Term::Union(i.clone(), j.clone()).match_strict(&Value::Int(3)),
            MatchResult::Pass(vec![])
        );
        assert_eq!(
            Term::Union(i.clone(), v.clone()).match_strict(&Value::Int(3)),
            MatchResult::Pass(vec![(fresh_var(), Value::Int(3))])
        );
        assert_eq!(
            Term::Union(i, j).match_strict(&Value::Tuple(jv, iv)),
            MatchResult::Fail
        );
    }
}
