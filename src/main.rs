mod ast;
mod error;
mod parse;
use error::ErrorContext;

fn main() {
    let mut context = ErrorContext::new();
    let intern = std::cell::RefCell::new(string_interner::StringInterner::new());
    for i in 0.. {
        let mut s = String::new();
        std::io::stdin().read_line(&mut s).unwrap();
        let result = parse::parse_str(&intern, &mut context, format!("<interactive:{}>", i), s);
        match result {
            Ok(result) => println!("{:#?}", result.into_iter().map(|x|x.format(&intern)).collect::<Vec<_>>()),
            Err(e) => context.write_error(e).unwrap(),
        }
    }
}
