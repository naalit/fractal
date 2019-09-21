mod ast;
mod error;
mod parse;
mod vm;
use error::ErrorContext;
use std::io::Write;
use vm::*;

fn main() {
    let mut context = ErrorContext::new();
    let intern = std::cell::RefCell::new(string_interner::StringInterner::new());
    let mut e = EvalContext::new(&intern);
    let mut i = 0;
    let mut buf = String::new();
    loop {
        if buf.is_empty() {
            print!(">> ");
            std::io::stdout().flush().unwrap();
        }
        let mut s = String::new();
        std::io::stdin().read_line(&mut s).unwrap();
        if !s.trim().is_empty() {
            buf.push_str(&s);
        } else if !buf.is_empty() {
            let result =
                parse::parse_str(&intern, &mut context, format!("<interactive:{}>", i), buf);
            buf = String::new();
            i += 1;
            match result {
                Ok(result) => match e.eval(result.into_iter().next().unwrap()) {
                    Ok(Value::Nil) => (),
                    Ok(x) => println!("{}", x),
                    Err(e) => {
                        let message = match e.val {
                            ErrorType::NotFound(s) => {
                                format!("Not found: '{}'", intern.borrow().resolve(s).unwrap())
                            }
                            ErrorType::MemberNotFound(ty, s) => format!(
                                "Not found: member '{}' of {:?}",
                                intern.borrow().resolve(s).unwrap(),
                                ty
                            ),
                            ErrorType::UnImplemented => format!("Feature not implemented"),
                        };
                        let error = error::Error::new(e.file, message, e.span, "");
                        context.write_error(error).unwrap()
                    }
                },
                Err(e) => context.write_error(e).unwrap(),
            }
        } else {
            break;
        }
    }
}
