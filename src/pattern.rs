use crate::ast::*;
use crate::vm::Value;
use string_interner::Sym;
use std::rc::Rc;
use std::sync::RwLock;
use lazy_static::lazy_static;

lazy_static! {
    // We're using an RwLock and not a RefCell because static forces it to be Sync
    static ref DEF: RwLock<usize> = RwLock::new(0);
}

pub fn verify(n: &Node, env: &mut Env<Rc<Total>>) -> bool {
    match &n.val {
        Term::App(f, x) => {
            let x = Rc::new(x.total(env));
            match &f.val {
                // TODO check for multiple branches that together provide exhaustiveness
                Term::Fun(f) => {
                    if !f.iter().all(|x| verify(&x.rhs, env)) {
                        return false;
                    }
                    let f: Vec<_> = f.iter().map(|p| p.lhs.total(env)).collect();
                    // TODO somehow verify pattern matches
                    f.iter().any(|p| p.will_match(&x).yes())
                }
                Term::Var(v) => if let Some(t) = env.get(*v) {
                    match &**t {
                        Total::Fun(v) => v.iter().any(|p| p.0.will_match(&x).yes()),
                        _ => false,
                    }
                } else {
                    false
                }
                _ => false,
            }
        }
        Term::Tuple(a,b) | Term::Inter(a,b) | Term::Union(a,b) => verify(a,env) && verify(b,env),
        Term::Fun(v) => v.iter().all(|Fun {lhs,rhs}| {
            let g = lhs.total(env).guaranteed();
            env.env.extend(g);
            verify(rhs, env)
        }),
        Term::Block(v) => {
            for i in v {
                if !verify(i, env) { return false; }
            }
            true
        }
        Term::Def(l, r) => match l.total(env).will_match(&Rc::new(r.total(env))) {
            WillMatch::Yes(v) => {
                env.env.extend(v);
                true
            }
            _ => false
        }
        _ => true,
    }
}

/// The set of possible patterns that a value can take
#[derive(Debug, PartialEq, Clone)]
pub enum Total {
    Fun(Vec<(Rc<Total>, Rc<Total>)>),
    Union(Rc<Total>, Rc<Total>),
    Inter(Rc<Total>, Rc<Total>),
    Unit(Value),
    Tuple(Rc<Total>, Rc<Total>),
    /// Recursive definitions use essentially nominal typing, where each definition is a unique type
    Rec(usize),
    /// Var() means that this wasn't bound when we created the Total, so it's on the pattern side and needs to be bound or it's invalid
    Var(Sym),
    Any,
}

pub enum WillMatch {
    Yes(Vec<(Sym,Rc<Total>)>),
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

impl Total {
    /// The totals that each variable is guaranteed to have if the pattern matches
    /// E.g. 'x:3' -> [(Sym(x), Total::Unit(Value::Int(3)))]
    pub fn guaranteed(&self) -> Vec<(Sym,Rc<Total>)> {
        match self {
            Total::Var(x) => vec![(*x, Rc::new(Total::Any))],
            Total::Tuple(a,b) => merge(a.guaranteed(), b.guaranteed()),
            Total::Inter(a,b) => {
                let mut bg = b.guaranteed();
                match &**a {
                    Total::Var(s) => {
                        bg.push((*s,Rc::clone(&b)));
                        bg
                    },
                    _ => bg,
                }
            }
            // TODO scenarios like 'A x | B x'
            _ => vec![],
        }
    }

    pub fn will_match(&self, other: &Rc<Total>) -> WillMatch {
        match (self,&**other) {
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
    fn total(&self, e: &Env<Rc<Total>>) -> Total;
}
impl HasTotal for Term {
    fn total(&self, env: &Env<Rc<Total>>) -> Total {
        if let Some(v) = self.simple() {
            Total::Unit(v)
        } else {
            match self {
                Term::Dot(x,s) => match &x.val {
                    Term::Int(_) | Term::Float(_) => (*env.modules[&Type::Num][s]).clone(),
                    _ => panic!("Old namespace system only supports numbers"),
                }
                Term::Var(s) => if let Some(t) = env.get(*s) {
                    (**t).clone()
                } else {
                    Total::Var(*s)
                }
                Term::Union(x, y) => Total::Union(Rc::new(x.total(env)), Rc::new(y.total(env))),
                Term::Fun(v) => Total::Fun(v.iter().map(|x| (Rc::new(x.lhs.total(env)), Rc::new(x.rhs.total(env)))).collect()),
                Term::Inter(x, y) => Total::Inter(Rc::new(x.total(env)), Rc::new(y.total(env))),
                _ => Total::Any,
            }
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

    fn fresh_file() -> codespan::FileId {
        codespan::Files::new().add("a", "a")
    }

    fn fresh_span() -> codespan::Span {
        codespan::Span::default()
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
        let file = fresh_file();
        let span = fresh_span();

        let i = Box::new(Node {
            file,
            span,
            val: Term::Int(2),
        });
        let j = Box::new(Node {
            file,
            span,
            val: Term::Int(3),
        });

        let iv = Box::new(Value::Int(2));
        let jv = Box::new(Value::Int(3));
        let ir = Rc::new(Total::Unit(Value::Int(2)));
        let jr = Rc::new(Total::Unit(Value::Int(3)));
        let vr = Rc::new(Total::Any);

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
        let file = fresh_file();
        let span = fresh_span();

        let i = Box::new(Node {
            file,
            span,
            val: Term::Int(2),
        });
        let j = Box::new(Node {
            file,
            span,
            val: Term::Int(3),
        });
        let v = Box::new(Node {
            file,
            span,
            val: Term::Var(fresh_var()),
        });

        let iv = Box::new(Value::Int(2));
        let jv = Box::new(Value::Int(3));
        let ir = Rc::new(Total::Unit(Value::Int(2)));
        let jr = Rc::new(Total::Unit(Value::Int(3)));
        let vr = Rc::new(Total::Any);

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
