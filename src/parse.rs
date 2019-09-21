use crate::ast::*;
use crate::error::*;
use codespan::FileId;
use codespan::Span;
use pest::iterators::{Pair, Pairs};
use pest::Parser;
use pest_derive::Parser;
use std::cell::RefCell;
use string_interner::{StringInterner, Sym};

pub type Intern = RefCell<StringInterner<Sym>>;

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
    pub fn format(self) -> &'static str {
        match self {
            Rule::dot => "'.'",
            Rule::tuple => "','",
            Rule::union => "'|'",
            Rule::app => "application",
            Rule::indent => "indent",
            Rule::dedent => "dedent",
            Rule::samedent => "matching indentation",
            Rule::line => "expression",
            Rule::num => "number",
            Rule::var | Rule::sym => "identifier",
            Rule::EOI => "end of input",
            Rule::close_paren => "')'",
            Rule::def => "'='",
            _ => "other",
        }
    }
}

fn pest_span(span: pest::Span) -> Span {
    Span::new(span.start() as u32, span.end() as u32)
}

pub fn parse_str(
    intern: &Intern,
    context: &mut ErrorContext,
    file: impl Into<String>,
    source: impl Into<String>,
) -> Result<Vec<Node>> {
    let source: String = source.into();
    let p = FractalPest::parse(Rule::prog, &source);
    let file = context.add_file(file, &source);
    match p {
        Ok(p) => pest_to_ast(file, intern, context, p),
        Err(e) => Err(Error::from_pest(e, file)),
    }
}

pub fn pest_to_ast(
    file: FileId,
    intern: &Intern,
    context: &ErrorContext,
    tree: Pairs<Rule>,
) -> Result<Vec<Node>> {
    fn f(file: FileId, intern: &Intern, context: &ErrorContext, p: Pair<Rule>) -> Result<Node> {
        let infix = |lhs: Result<Node>, op: Pair<Rule>, rhs: Result<Node>| -> Result<Node> {
            let lhs = lhs?;
            let rhs = rhs?;
            let span = lhs.span.merge(rhs.span);
            let val = match op.as_rule() {
                Rule::tuple => Term::Tuple(Box::new(lhs), Box::new(rhs)),
                Rule::union => Term::Union(Box::new(lhs), Box::new(rhs)),
                Rule::def => Term::Def(Box::new(lhs), Box::new(rhs)),
                Rule::dot => {
                    if let Term::Var(s) = rhs.val {
                        Term::Dot(Box::new(lhs), s)
                    } else {
                        return Err(Error::new(
                            file,
                            "Parse error: expected identifier after '.'",
                            rhs.span,
                            "expected identifier",
                        ));
                    }
                }
                Rule::app => Term::App(Box::new(lhs), Box::new(rhs)),
                Rule::var | Rule::sym => {
                    let sym = intern.borrow_mut().get_or_intern(op.as_str().trim());
                    let span = lhs.span.merge(pest_span(op.as_span()));
                    Term::App(
                        Box::new(Node {
                            val: Term::Dot(Box::new(lhs), sym),
                            file,
                            span,
                        }),
                        Box::new(rhs),
                    )
                }
                x => panic!("Unknown op {:?}", x),
            };
            Ok(Node { file, span, val })
        };
        let span = pest_span(p.as_span());
        let val = match p.as_rule() {
            Rule::num => {
                let s = p.as_str().trim();
                if let Ok(i) = s.parse::<i32>() {
                    Term::Int(i)
                } else if let Ok(f) = s.parse::<f32>() {
                    Term::Float(f)
                } else {
                    panic!("We can't parse something that matched the 'num' rule!")
                }
            }
            Rule::var | Rule::sym => Term::Var(intern.borrow_mut().get_or_intern(p.as_str())),
            Rule::line => {
                use pest::prec_climber::*;
                let ops = vec![
                    Operator::new(Rule::def, Assoc::Left),
                    Operator::new(Rule::union, Assoc::Right),
                    Operator::new(Rule::var, Assoc::Left),
                    Operator::new(Rule::sym, Assoc::Left),
                    Operator::new(Rule::app, Assoc::Left),
                    Operator::new(Rule::tuple, Assoc::Right),
                    Operator::new(Rule::dot, Assoc::Left),
                ];
                let climber = PrecClimber::new(ops);
                // The '.val' here is a little annoying - we're taking a node, taking out the term, and converting it back to a node
                // However, I don't know that there's really a better way
                climber
                    .climb(
                        p.into_inner().filter(filter_silent),
                        |x| f(file, intern, context, x),
                        infix,
                    )?
                    .val
            }
            x => panic!("Unknown primary rule {:?}", x),
        };
        Ok(Node { file, span, val })
    }
    let mut v = Vec::new();
    for i in tree.filter(filter_silent) {
        let x = f(file, intern, context, i)?;
        v.push(x);
    }
    Ok(v)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn clean_parse(s: &str) -> Result<Vec<Node>> {
        let intern = RefCell::new(StringInterner::new());
        let mut context = ErrorContext::new();
        let p = parse_str(&intern, &mut context, "test", s);
        match &p {
            Ok(_) => (),
            Err(e) => context.write_error(e.clone()).unwrap(),
        };
        p
    }

    macro_rules! assert_matches {
        (@ok $p:ident => $q:pat) => {
            match &$p.val {
                $q => (),
                x => panic!("{:?} doesn't match {}", x, stringify!($p => $q)),
            }
        };
        (@ok $p:ident => $q:pat, $($p1:ident => $q1:pat),*) => {
            match &$p.val {
                $q => assert_matches!(@ok $($p1 => $q1),*),
                x => panic!("{:?} doesn't match {}", x, stringify!($p => $q)),
            }
        };
        ($start:expr => { $p:ident => $q:pat, $($x:tt)* }) => {
            match $start {
                Ok(p) => {
                    let $p = &p[0];
                    match &$p.val {
                        $q => assert_matches!(@ok $($x)*),
                        x => panic!("{:?} doesn't match {}", x, stringify!($p => $q)),
                    }
                },
                _ => panic!("Parse failed"),
            }
        };
    }

    #[test]
    fn test_def_prec() {
        assert_matches! {
            clean_parse("a = 2 + 4") => {
                x => Term::Def(a,x),
                x => Term::App(x,four),
                x => Term::Dot(two,_),
                four => Term::Int(4),
                two => Term::Int(2),
                a => Term::Var(_)
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
                a => Term::Var(_),
                c => Term::Var(_),
                e => Term::Var(_),

                hij => Term::App(hi,j),
                hi => Term::Dot(h,_),
                h => Term::Var(_),
                j => Term::Var(_)
            }
        }
    }

    #[test]
    fn test_app_tuple() {
        assert_matches! {
            clean_parse("f x, y") => {
                p => Term::App(f, t),
                f => Term::Var(_),
                t => Term::Tuple(x, y),
                x => Term::Var(_),
                y => Term::Var(_)
            }
        };
    }
}
