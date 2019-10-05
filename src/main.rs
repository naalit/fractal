mod ast;
mod error;
mod parse;
mod pattern;
mod vm;
mod builtin;
use error::ErrorContext;
use std::io::Read;
use std::io::Write;
use vm::*;

fn main() {
    let mut context = ErrorContext::new();
    let intern = std::cell::RefCell::new(string_interner::StringInterner::new());
    let mut e = EvalContext::new();
    let mut i = 0;
    let mut buf = String::new();

    let args: Vec<String> = std::env::args().collect();
    let in_file = args.iter().find(|x| x.rfind(".fl").is_some());
    let verbose = args
        .iter()
        .any(|x| x.trim() == "-v" || x.trim() == "--verbose");

    let mut env = builtin::totals();

    if let Some(f) = in_file {
        let mut s = String::new();
        std::fs::File::open(f)
            .expect("Couldn't open file")
            .read_to_string(&mut s)
            .unwrap();
        match parse::parse_str(&mut context, f, s) {
            Ok(result) => {
                for i in result {
                    if verbose {
                        println!("{}", i.format());
                    }
                    match pattern::verify(&i, &mut env) {
                        Ok(()) if verbose => println!("Checks out!"),
                        Ok(()) => (),
                        Err(e) => {
                            for i in e.error() {
                                context.write_error(i).unwrap();
                            }
                            continue
                        },
                    }
                    match e.eval(i) {
                        Ok(Value::Nil) => (),
                        Ok(x) => println!("{}", x),
                        Err(e) => {
                            println!("Runtime error!");
                            let message = match e.val {
                                ErrorType::MatchError => "Match failed".to_string(),
                                ErrorType::NotFound(s) => {
                                    format!("Not found: '{}'", intern.borrow().resolve(s).unwrap())
                                }
                                ErrorType::MemberNotFound(ty, s) => format!(
                                    "Not found: member '{}' of {:?}",
                                    intern.borrow().resolve(s).unwrap(),
                                    ty
                                ),
                                ErrorType::UnImplemented => "Feature not implemented".to_string(),
                            };
                            let error = error::Error::new(e.file, message, e.span, "");
                            context.write_error(error).unwrap()
                        }
                    }
                }
            }
            Err(e) => context.write_error(e).unwrap(),
        }
    } else {
        // TODO switch to rustyline
        loop {
            if buf.is_empty() {
                print!(">> ");
                std::io::stdout().flush().unwrap();
            }
            let mut s = String::new();
            if std::io::stdin().read_line(&mut s).unwrap() == 0 || s.trim() == "exit" {
                println!("Goodbye!");
                break;
            }

            // If we can parse this line, do that;
            // If not, assume it's multiple lines and stop when they give us a blank line
            let result = if buf.is_empty() {
                match parse::parse_str(&mut context, format!("<interactive:{}>", i), &s) {
                    r @ Ok(_) => r,
                    Err(_) => {
                        buf.push_str(&s);
                        continue;
                    }
                }
            } else if s.trim().is_empty() {
                parse::parse_str(&mut context, format!("<interactive:{}>", i), buf)
            } else {
                buf.push_str(&s);
                continue;
            };

            buf = String::new();
            i += 1;
            match result {
                Ok(result) => {
                    for i in result {
                        if verbose {
                            println!("{}", i.format());
                        }
                        match pattern::verify(&i, &mut env) {
                            Ok(()) => println!("Checks out!"),
                            Err(e) => {
                                println!("Match error {:?}", e);
                                for i in e.error() {
                                    context.write_error(i).unwrap();
                                }
                                continue
                            },
                        }
                        match e.eval(i) {
                            Ok(Value::Nil) => (),
                            Ok(x) => println!("{}", x),
                            Err(e) => {
                                println!("Runtime error!");
                                let message = match e.val {
                                    ErrorType::MatchError => "Match failed".to_string(),
                                    ErrorType::NotFound(s) => format!(
                                        "Not found: '{}'",
                                        intern.borrow().resolve(s).unwrap()
                                    ),
                                    ErrorType::MemberNotFound(ty, s) => format!(
                                        "Not found: member '{}' of {:?}",
                                        intern.borrow().resolve(s).unwrap(),
                                        ty
                                    ),
                                    ErrorType::UnImplemented => {
                                        "Feature not implemented".to_string()
                                    }
                                };
                                let error = error::Error::new(e.file, message, e.span, "");
                                context.write_error(error).unwrap()
                            }
                        }
                    }
                }
                Err(e) => context.write_error(e).unwrap(),
            }
        }
    }
}
