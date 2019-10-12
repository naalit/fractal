//! Indentation still doesn't quite work the way I want it to, so I'll probably switch to a handwritten parser eventually
//! For example, try:
//! ```notest
//! do
//!        var x = 3
//!    var y = 4
//! ```
//! It should give an error, but it doesn't. Or try
//! ```notest
//! 3
//!   + 2
//!   .sqr
//! ```
//! Which resolves to `3 + (2.sqr)` instead of `(3 + 2).sqr`
//! So it returns `7` instead of `25`

use crate::ast::*;
use crate::common::*;
use crate::error::*;
use codespan::FileId;
use codespan::Span;
use pest::iterators::{Pair, Pairs};
use pest::Parser;
use pest_derive::Parser;
use std::rc::Rc;

#[derive(Parser)]
#[grammar = "fractal.pest"]
pub struct FractalPest;

/// Removes indentation and other silent rules that can't be made silent,
/// either because they're atomic or we want them to appear in error messages
fn filter_silent(x: &Pair<Rule>) -> bool {
    use Rule::*;
    match x.as_rule() {
        indent | dedent | samedent | EOI | close_paren => false,
        _ => true,
    }
}

impl Rule {
    /// We want to show nice error messages
    pub fn format(self) -> &'static str {
        match self {
            Rule::dot => "'.'",
            Rule::tuple => "','",
            Rule::union => "'|'",
            Rule::inter => ":",
            Rule::app => "application",
            Rule::indent => "indent",
            Rule::dedent => "dedent",
            Rule::samedent => "matching indentation",
            Rule::line => "expression",
            Rule::num => "number",
            Rule::var => "'var'",
            Rule::ident | Rule::sym => "identifier",
            Rule::EOI => "end of input",
            Rule::close_paren => "')'",
            Rule::def => "'='",
            Rule::keyword => "keyword",
            Rule::fun => "function",
            Rule::rec => "rec",
            Rule::fun_pat | Rule::fun_arm => "pattern",
            Rule::nil => "()",
            _ => "other",
        }
    }
}

fn pest_span(span: pest::Span) -> Span {
    Span::new(span.start() as u32, span.end() as u32)
}

pub fn parse_str(
    context: &mut ErrorContext,
    file: impl Into<String>,
    source: impl Into<String>,
) -> Result<Vec<BTerm>> {
    let source: String = source.into();
    let p = FractalPest::parse(Rule::prog, &source);
    let file = context.add_file(file, &source);
    match p {
        Ok(p) => pest_to_ast(file, context, p),
        Err(e) => Err(Error::from_pest(e, file)),
    }
}

fn parse_fun(file: FileId, context: &ErrorContext, tree: Pairs<Rule>) -> Result<Vec<Fun>> {
    let mut v = Vec::new();
    for i in tree.filter(filter_silent) {
        let f = match i.as_rule() {
            Rule::fun_arm => {
                let mut i = i.into_inner();
                let pat = i.next().unwrap();
                let pat = parse_line(file, context, pat)?;
                let body = i.next().unwrap();
                let body = parse_line(file, context, body)?;
                Fun {
                    lhs: pat,
                    rhs: body,
                }
            }
            x => panic!("Rule {:?} in fun!", x),
        };
        v.push(f);
    }
    Ok(v)
}

fn parse_line(file: FileId, context: &ErrorContext, p: Pair<Rule>) -> Result<BTerm> {
    let infix = |lhs: Result<BTerm>, op: Pair<Rule>, rhs: Result<BTerm>| -> Result<BTerm> {
        let lhs = lhs?;
        let rhs = rhs?;
        let span = lhs.span.merge(rhs.span);
        let val = match op.as_rule() {
            Rule::tuple => Term::Tuple(lhs, rhs),
            Rule::union => Term::Union(lhs, rhs),
            Rule::inter => Term::Inter(lhs, rhs),
            Rule::def => Term::Def(lhs, rhs),
            // Rule::fun_sym => Term::Fun(Box::new(lhs), Box::new(rhs)),
            Rule::dot => {
                if let Term::Ident(s) = &*rhs.val {
                    Term::Dot(lhs, *s)
                } else {
                    return Err(Error::new(
                        file,
                        "Parse error: expected identifier after '.'",
                        rhs.span,
                        "expected identifier",
                    ));
                }
            }
            Rule::app => Term::App(lhs, rhs),
            Rule::ident | Rule::sym => {
                let sym = INTERN.write().unwrap().get_or_intern(op.as_str().trim());
                let span = lhs.span.merge(pest_span(op.as_span()));
                Term::App(
                    Node {
                        val: Rc::new(Term::Dot(lhs, sym)),
                        file,
                        span,
                    },
                    rhs,
                )
            }
            x => panic!("Unknown op {:?}", x),
        };
        Ok(Node {
            file,
            span,
            val: Rc::new(val),
        })
    };
    let span = pest_span(p.as_span());
    let val = match p.as_rule() {
        Rule::nil => Term::Lit(Literal::Nil),
        Rule::num => {
            let s = p.as_str().trim();
            if let Ok(i) = s.parse::<i32>() {
                Term::Lit(Literal::Int(i))
            } else if let Ok(f) = s.parse::<f32>() {
                Term::Lit(Literal::Float(f))
            } else {
                panic!("We can't parse something that matched the 'num' rule!")
            }
        }
        // Remove the 'rec' keyword, but don't bother recursing; we know there's a 'var' in there.
        Rule::rec => Term::Rec(
            INTERN
                .write()
                .unwrap()
                .get_or_intern(p.as_str().trim()[3..].trim()),
        ),
        Rule::var => Term::Var(
            INTERN
                .write()
                .unwrap()
                .get_or_intern(p.as_str().trim()[3..].trim()),
        ),
        Rule::ident | Rule::sym => Term::Ident(INTERN.write().unwrap().get_or_intern(p.as_str())),
        Rule::fun => {
            let arms = parse_fun(file, context, p.into_inner())?;
            Term::Fun(arms)
        }
        Rule::block => Term::Block(
            p.into_inner()
                .filter(filter_silent)
                .map(|x| parse_line(file, context, x))
                .try_fold(Vec::new(), |mut acc, x| {
                    acc.push(x?);
                    Ok(acc)
                })?,
        ),
        Rule::line | Rule::fun_pat => {
            use pest::prec_climber::*;
            let ops = vec![
                Operator::new(Rule::def, Assoc::Left),
                Operator::new(Rule::union, Assoc::Right),
                Operator::new(Rule::ident, Assoc::Left),
                Operator::new(Rule::sym, Assoc::Left),
                Operator::new(Rule::app, Assoc::Left),
                Operator::new(Rule::tuple, Assoc::Right),
                Operator::new(Rule::inter, Assoc::Right),
                Operator::new(Rule::dot, Assoc::Left),
            ];
            let climber = PrecClimber::new(ops);
            // The '.val' here is a little annoying - we're taking a node, taking out the term, and converting it back to a node
            // However, I don't know that there's really a better way
            Rc::try_unwrap(
                climber
                    .climb(
                        p.into_inner().filter(filter_silent),
                        |x| parse_line(file, context, x),
                        infix,
                    )?
                    .val,
            )
            .unwrap()
        }
        x => panic!("Unknown primary rule {:?}", x),
    };
    Ok(Node {
        file,
        span,
        val: Rc::new(val),
    })
}

pub fn pest_to_ast(file: FileId, context: &ErrorContext, tree: Pairs<Rule>) -> Result<Vec<BTerm>> {
    let mut v = Vec::new();
    for i in tree.filter(filter_silent) {
        let x = parse_line(file, context, i)?;
        v.push(x);
    }
    Ok(v)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn clean_parse(s: &str) -> Result<Vec<BTerm>> {
        let mut context = ErrorContext::new();
        let p = parse_str(&mut context, "test", s);
        match &p {
            Ok(_) => (),
            Err(e) => context.write_error(e.clone()).unwrap(),
        };
        p
    }

    macro_rules! assert_matches {
        (@ok $p:expr => $q:pat) => {
            match &*$p.val {
                $q => (),
                x => panic!("{:?} doesn't match {}", x, stringify!($p => $q)),
            }
        };
        (@ok $p:expr => $q:pat, $($p1:expr => $q1:pat),*) => {
            match &*$p.val {
                $q => assert_matches!(@ok $($p1 => $q1),*),
                x => panic!("{:?} doesn't match {}", x, stringify!($p => $q)),
            }
        };
        ($start:expr => { $p:ident => $q:pat, $($x:tt)* }) => {
            match $start {
                Ok(p) => {
                    let $p = &p[0];
                    match &*$p.val {
                        $q => assert_matches!(@ok $($x)*),
                        x => panic!("{:?} doesn't match {}", x, stringify!($p => $q)),
                    }
                },
                _ => panic!("Parse failed"),
            }
        };
    }

    #[test]
    fn test_comment() {
        assert_matches! {
            clean_parse(r#"
            zero # One
            # Two
             = # Three
            # Four
              5 # Six
            # Seven
            "#) => {
                x => Term::Def(z,f),
                z => Term::Ident(_),
                f => Term::Lit(Literal::Int(5))
            }
        }
    }

    #[test]
    fn test_inter() {
        assert_matches! {
            // This should be prec = fun (S (x: Nat)) => x
            clean_parse("prec = fun\n  S x : Nat => x") => {
                x => Term::Def(prec, x),
                prec => Term::Ident(_),
                x => Term::Fun(f),
                f[0].lhs => Term::App(s, xnat),
                s => Term::Ident(_),
                xnat => Term::Inter(x, nat),
                x => Term::Ident(_),
                nat => Term::Ident(_),
                f[0].rhs => Term::Ident(_)
            }
        }
    }

    #[test]
    fn test_block() {
        assert_matches! {
            clean_parse("f do\n x = do\n  y = 2\n y = 3") => {
                x => Term::App(f,b),
                f => Term::Ident(_),
                b => Term::Block(v),
                v[0] => Term::Def(x,b2),
                x => Term::Ident(_),
                b2 => Term::Block(v2),
                v2[0] => Term::Def(y,two),
                y => Term::Ident(_),
                two => Term::Lit(Literal::Int(2)),
                v[1] => Term::Def(y,three),
                y => Term::Ident(_),
                three => Term::Lit(Literal::Int(3))
            }
        }
    }

    #[test]
    fn test_fun() {
        assert_matches! {
            clean_parse("f = fun x y => y z") => {
                x => Term::Def(f,fun),
                f => Term::Ident(_),
                fun => Term::Fun(fs),
                fs[0].lhs => Term::App(x,y),
                fs[0].rhs => Term::App(y2,z),
                x => Term::Ident(_),
                y => Term::Ident(_),
                y2 => Term::Ident(_),
                z => Term::Ident(_)
            }
        }
    }

    #[test]
    fn test_def_prec() {
        assert_matches! {
            clean_parse("a = 2 + 4") => {
                x => Term::Def(a,x),
                x => Term::App(x,four),
                x => Term::Dot(two,_),
                four => Term::Lit(Literal::Int(4)),
                two => Term::Lit(Literal::Int(2)),
                a => Term::Ident(_)
            }
        }
    }

    #[test]
    fn test_app_infix() {
        assert_matches! {
            clean_parse("f 2 + f 3") => {
                x => Term::App(f2p, f3),
                f3 => Term::App(f, three),
                f => Term::Ident(_),
                three => Term::Lit(Literal::Int(3)),
                f2p => Term::Dot(f2, _),
                f2 => Term::App(f, two),
                f => Term::Ident(_),
                two => Term::Lit(Literal::Int(2))
            }
        }
    }

    #[test]
    fn test_indent() {
        assert_matches! {
            // a.b(c)
            //  .d(e)
            //  .g(h.i(j))
            clean_parse(r#"a b c
                d e
                g
                    h i j"#) => {
                x => Term::App(x,hij),

                x => Term::Dot(x,_),
                x => Term::App(abcd,e),
                abcd => Term::Dot(abc,_),
                abc => Term::App(ab,c),
                ab => Term::Dot(a,_),
                a => Term::Ident(_),
                c => Term::Ident(_),
                e => Term::Ident(_),

                hij => Term::App(hi,j),
                hi => Term::Dot(h,_),
                h => Term::Ident(_),
                j => Term::Ident(_)
            }
        }
    }

    #[test]
    fn test_app_tuple() {
        assert_matches! {
            clean_parse("f x, y") => {
                p => Term::App(f, t),
                f => Term::Ident(_),
                t => Term::Tuple(x, y),
                x => Term::Ident(_),
                y => Term::Ident(_)
            }
        };
    }
}
