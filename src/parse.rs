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
            Rule::var => "identifier",
            Rule::EOI => "end of input",
            Rule::close_paren => "')'",
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
                Rule::var => {
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
            Rule::num => Term::Int(p.as_str().trim().parse().unwrap()),
            Rule::var => Term::Var(intern.borrow_mut().get_or_intern(p.as_str())),
            Rule::line => {
                use pest::prec_climber::*;
                let ops = vec![
                    Operator::new(Rule::union, Assoc::Right),
                    Operator::new(Rule::var, Assoc::Left),
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


// macro_rules! start_end {
//     ($s:ident, $e:ident, $v:ident, $pair:ident) => {{
//         $v.$s();
//         visit_pest($pair.into_inner(), $v);
//         $v.$e();
//     }};
// }

// pub fn visit_pest(tree: Pairs<Rule>, visitor: &mut impl Visitor) {
//     let mut intern = StringInterner::new();
//     for pair in tree {
//         match pair.as_rule() {
//             Rule::ident => visitor.visit_var(intern.get_or_intern(pair.as_str().trim())),
//             Rule::number => match pair.as_str().trim().parse::<i32>() {
//                 Ok(i) => visitor.visit_int(i),
//                 Err(_) => visitor.visit_float(pair.as_str().trim().parse().unwrap()),
//             }
//             Rule::tuple => {
//                 visitor.start_tuple();
//                 visit_pest(pair.into_inner(), visitor);
//                 visitor.end_tuple();
//             }
//             Rule::union => start_end!(start_union, end_union, visitor, pair),
//             Rule::block => start_end!(start_block, end_block, visitor, pair),
//             Rule::app | Rule::app_b => start_end!(start_app, end_app, visitor, pair),
//             Rule::fun => {
//                 visitor.start_fun();
//                 let mut first = true;
//                 for pair in pair.into_inner() {
//                     if let Rule::arm = pair.as_rule() {
//                         if !first {
//                             visitor.next_arm();
//                         }
//                         visit_pest(pair.into_inner(), visitor);
//                         first = false;
//                     }
//                 }
//                 visitor.end_fun();
//             }
//             Rule::def => {
//                 visitor.def_lhs();
//                 let mut p = pair.into_inner();
//                 visit_pest(Pairs::single(p.next().unwrap()), visitor);
//                 visitor.def_rhs();
//                 visit_pest(Pairs::single(p.next().unwrap()), visitor);
//                 visitor.end_def();
//             }
//             Rule::statement => visit_pest(pair.into_inner(), visitor),
//             Rule::EOI => (),
//             _ => panic!("Found unexpected rule {:?} in parse tree", pair.as_rule()),
//         }
//     }
// }
