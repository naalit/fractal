use crate::ast::*;
use crate::builtin::Builtin;
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
            MatchError::Custom(s) => vec![Error::new(s.file, &*s, s.span, "")],
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

/// The set of possible patterns that a value can take
/// This is the IR that we use for partial evaluation and validation
/// So it can be used for general evaluation as well
#[derive(Debug, PartialEq, Clone)]
pub enum Total {
    /// App(f, x, what it will return)
    App(BTotal, BTotal, BTotal),
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
    /// They're not implemented yet, though, so really who knows how they'll end up working
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
            Total::Defined(s, t) => {
                write!(f, "{}: {}", INTERN.read().unwrap().resolve(*s).unwrap(), t)
            }
            Total::Var(s) => write!(f, "{}", INTERN.read().unwrap().resolve(*s).unwrap()),
            Total::Rec(u) => write!(f, "<deprecated Rec {}>", u),
            Total::App(a, b, c) => write!(f, "(({})({}) : {})", a, b, c),
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
            }
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

pub fn either(a: WillMatch, b: WillMatch) -> WillMatch {
    match (a, b) {
        (Ok(x), _) => Ok(x),
        (_, Ok(x)) => Ok(x),
        (Err(e), Err(e2)) => Err(MatchError::Multi(vec![e, e2])),
    }
}

/// A helper function to concatenate two `Vec`s
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
            Total::Defined(_, a) => a.simple(),
            _ => false,
        }
    }

    fn args(&self) -> BTotal {
        match &**self {
            // A union of all the possible arguments
            Total::Fun(v) => {
                let mut u = Node::new_raw(Total::Void);
                for (pat, _) in v {
                    if let Total::Void = &*u {
                        u = pat.clone();
                    } else {
                        u = self.replace(Total::Union(pat.clone(), u));
                    }
                }
                u
            }
            Total::Builtin(b) => self.replace(b.args()),
            // Not callable
            _ => self.replace(Total::Void),
        }
    }

    /// If we match `(x, something)` return `something`, otherwise Err
    fn match_tuple(&self, x: &BTotal) -> Result<BTotal, MatchError> {
        match &**self {
            Total::Tuple(a, b) => match a.will_match(x) {
                Ok(_) => Ok(b.clone()),
                Err(e) => Err(e),
            },
            // Either works
            Total::Union(a, b) => match a.match_tuple(x) {
                Ok(a) => match b.match_tuple(x) {
                    Ok(b) => Ok(self.replace(Total::Union(a, b))),
                    Err(_) => Ok(a.clone()),
                },
                Err(e) => match b.match_tuple(x) {
                    Ok(b) => Ok(b.clone()),
                    Err(e2) => Err(MatchError::Multi(vec![e, e2])),
                },
            },
            Total::Inter(a, b) => match a.match_tuple(x) {
                Ok(a) => match b.match_tuple(x) {
                    Ok(b) => Ok(self.replace(Total::Inter(a, b))),
                    Err(e) => Err(e),
                },
                Err(e) => Err(e),
            },
            Total::Any => Ok(self.clone()),
            Total::Var(_) => Ok(self.replace(Total::Any)),
            Total::Defined(_, a) | Total::App(_, _, a) => a.match_tuple(x),
            _ => Err(MatchError::Pat(self.clone(), x.clone())),
        }
    }

    /// If this is on the pattern side, what could it possibly match?
    fn pat(&self) -> BTotal {
        match &**self {
            Total::Var(_) => self.replace(Total::Any),
            Total::Tuple(a, b) => self.replace(Total::Tuple(a.pat(), b.pat())),
            Total::Union(a, b) => self.replace(Total::Or(a.pat(), b.pat())),
            Total::Inter(a, b) => self.replace(Total::Inter(a.pat(), b.pat())),
            Total::App(_, _, _) | Total::Rec(_) | Total::Num | Total::Or(_, _) => {
                self.replace(Total::Void)
            }
            Total::Defined(_, x) => x.pat(),
            Total::Builtin(b) => self.replace(b.pat()),
            // TODO
            Total::Fun(_) => self.replace(Total::Any),
            Total::Any | Total::Void | Total::Lit(_) => self.clone(),
        }
    }

    /// The totals that each variable is guaranteed to have if the pattern matches
    /// E.g. `var x:3` -> `[(Sym(x), Total::Lit(Literal::Int(3)))]`
    pub fn guaranteed(&self) -> Vec<(Sym, BTotal)> {
        match &**self {
            Total::Var(x) => vec![(*x, self.replace(Total::Any))],
            Total::Tuple(a, b) => merge(a.guaranteed(), b.guaranteed()),
            Total::Inter(a, b) => {
                // HACK
                let mut bg = b.guaranteed();
                match &**a {
                    Total::Var(s) => {
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

                    for (k, v) in sub {
                        rem.push(k);
                        if let Some(v) = env.env.insert(k, v) {
                            old.push((k, v));
                        }
                    }

                    body.simplify_mut(env);
                    body.move_env_mut();

                    for k in rem {
                        env.env.remove(&k);
                    }
                    for (k, v) in old {
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
        if other.simple() && a.simple() {
            Ok(a)
        } else if self.span.end() - other.span.start() < codespan::ByteOffset(3) {
            Ok(Node::new(
                codespan::Span::new(self.span.start(), other.span.end()),
                self.file,
                Total::App(self, other, a),
            ))
        } else {
            Ok(other.replace(Total::App(self, other.clone(), a)))
        }
    }

    pub fn move_env(&self) -> Option<BTotal> {
        let multi = |cons: fn(BTotal, BTotal) -> Total, a: &BTotal, b: &BTotal| match a.move_env() {
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
                    if !(t == u || t.will_match(u).is_ok()) {
                        println!("Warning: Total::Defined was narrowed to {}, which doesn't match the previous value of {}", u, t);
                    }
                    Some(self.replace(Total::Defined(*s, u.clone())))
                }
                None => None,
            },
            Total::Inter(a, b) => multi(Total::Inter, a, b),
            Total::Tuple(a, b) => multi(Total::Tuple, a, b),
            Total::Union(a, b) => multi(Total::Union, a, b),
            Total::App(a, b, _) => {
                let a = a.simplify(env).unwrap_or_else(|| a.clone());
                let b = b.simplify(env).unwrap_or_else(|| b.clone());
                let app = a
                    .app(b, env)
                    .expect("Could not apply a and b in simplify()!");
                Some(app)
            }
            // TODO Fun
            _ => None,
        }
    }

    pub fn will_match(&self, other: &BTotal) -> WillMatch {
        match (&**self, &**other) {
            (_, Total::Inter(a, b)) => either(self.will_match(a), self.will_match(b)),
            (_, Total::Or(a, b)) => both(self.will_match(a), self.will_match(b)),
            (Total::Defined(_, t), _) => t.will_match(other),
            (_, Total::Defined(_, t)) => self.will_match(t),
            (_, Total::App(_, _, r)) => self.will_match(r),
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
            // `Total::Or` shouldn't get here normally - `.pat()` would replace it with `Total::Void`
            // It's treated as a union, though, because we have a sanity check on `Total::Defined` narrowing
            //  that makes sure the new value matches the old one, and that requires this
            (Total::Union(a, b), _) | (Total::Or(a, b), _) => match a.will_match(other) {
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
impl HasTotal for BTerm {
    // TODO merge with verify
    fn total(&self, env: &mut Env<BTotal>, pat: bool) -> Result<BTotal, MatchError> {
        let Node { span, file, .. } = self;
        let t = match &**self {
            Term::Lit(l) => Ok(Total::Lit(*l)),
            Term::Dot(n, s) => resolve_dot(
                n.total(env, pat)?,
                Node::new(
                    codespan::Span::new(n.span.end(), self.span.end()),
                    self.file,
                    *s,
                ),
                env,
            )
            .map(|x| (*x).clone()),
            Term::Tuple(x, y) => {
                let (x, y) = and(x.total(env, pat), y.total(env, pat))?;
                Ok(Total::Tuple(x, y))
            }
            Term::Ident(s) => {
                if let Some(t) = env.get(*s) {
                    Ok(Total::Defined(*s, t.clone()))
                } else {
                    Err(MatchError::NotFound(Node::new(*span, *file, *s)))
                }
            }
            Term::Var(s) => {
                if pat {
                    Ok(Total::Var(*s))
                } else {
                    Err(MatchError::Custom(self.replace(String::from(
                        "`var` is only allowed on the pattern side",
                    ))))
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
                    Err(MatchError::Custom(self.replace(String::from(
                        "Intersections are only allowed on the pattern side",
                    ))))
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
                // TODO make blocks work in functions - test:
                // ```
                // var f = fun var x:num => do
                //   var y = x
                //   y
                // f 3 # Should print 3
                // ```
                let mut t = Node::new(*span, *file, Total::Lit(Literal::Nil));
                // TODO this is slow, probably
                let mut env2 = env.clone();
                for i in v {
                    t = i.total(&mut env2, pat)?;
                }
                return Ok(t);
            }
            // TODO remove rec
            Term::Rec(_) => Ok(Total::Any),
        }?;
        Ok(Node::new(*span, *file, t))
    }
}

/// To resolve `x.s`, we do, in order:
/// - Is `x` a record with member `s`? If so, return the member `s` of `x`.
/// - Is `x` a tag, and there's something named `s` in the module hierarchy where it was defined? If so, pass it `x`.
/// - Is there something named `s` in the current module hierarchy? If so, pass it `x`.
/// If we try to pass `x` to something that doesn't accept it but does accept `(x, _)`, we curry it.
/// Currently, this function only implements the last one. (We don't even have records or tags yet!)
fn resolve_dot(x: BTotal, s: Node<Sym>, env: &mut Env<BTotal>) -> Result<BTotal, MatchError> {
    match env.get(*s) {
        Some(f) => {
            let f = f.clone();
            match f.clone().app(x.clone(), env) {
                Ok(a) => Ok(a),
                Err(_) => match f.args().match_tuple(&x) {
                    Ok(rest) => Ok(x.replace(Total::Fun(vec![(
                        x.replace(Total::Inter(x.replace(Total::Var(*s)), rest.clone())),
                        f.clone().app(
                            x.replace(Total::Tuple(
                                x.clone(),
                                x.replace(Total::Defined(*s, rest.pat())),
                            )),
                            env,
                        )?,
                    )]))),
                    Err(e) => Err(e),
                },
            }
        }
        None => Err(MatchError::MemberNotFound(x, s)),
    }
}
