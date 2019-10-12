mod ast;
mod builtin;
mod common;
mod error;
mod parse;
mod pattern;
use common::*;
use error::ErrorContext;
use std::io::Read;
use std::io::Write;

fn main() {
    let mut context = ErrorContext::new();
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
                        println!("{}", i);
                    }
                    use pattern::HasTotal;
                    match i.total(&mut env, false) {
                        Ok(mut x) => {
                            x.simplify_mut(&mut env);
                            // We only print the result of a script with -v
                            if verbose {
                                println!("=> {}", x);
                            }
                        }
                        Err(e) => {
                            println!("Match error {:?}", e);
                            for i in e.error() {
                                context.write_error(i).unwrap();
                            }
                        }
                    }
                }
            }
            Err(e) => context.write_error(e).unwrap(),
        }
    } else {
        // I copied this from clap: https://kbknapp.github.io/clap-rs/src/clap/macros.rs.html#632-640
        macro_rules! crate_version {
            () => {
                format!(
                    "{}.{}.{}{}",
                    env!("CARGO_PKG_VERSION_MAJOR"),
                    env!("CARGO_PKG_VERSION_MINOR"),
                    env!("CARGO_PKG_VERSION_PATCH"),
                    option_env!("CARGO_PKG_VERSION_PRE").unwrap_or("")
                )
            };
        };
        println!("Fractal {}", crate_version!());
        // TODO switch to rustyline
        loop {
            if buf.is_empty() {
                print!(">> ");
                std::io::stdout().flush().unwrap();
            }
            let mut s = String::new();
            if std::io::stdin().read_line(&mut s).unwrap() == 0 || s.trim() == "exit" {
                println!("\nGoodbye!");
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
                            println!("{}", i);
                        }
                        use pattern::HasTotal;
                        match i.total(&mut env, false) {
                            Ok(mut x) => {
                                x.simplify_mut(&mut env);
                                println!("=> {}", x);
                            }
                            Err(e) => {
                                println!("Match error {:?}", e);
                                for i in e.error() {
                                    context.write_error(i).unwrap();
                                }
                            }
                        }
                    }
                }
                Err(e) => context.write_error(e).unwrap(),
            }
        }
    }
}
