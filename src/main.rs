mod ast;
mod builtin;
mod common;
mod error;
mod parse;
mod pattern;
use common::*;
use std::io::Read;

fn repl(verbose: bool) {
    let mut i = 0;
    let mut buf = String::new();

    let mut env = builtin::totals();

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

    let config = rustyline::Config::builder()
        .auto_add_history(true)
        .tab_stop(2)
        .build();
    let mut rl = rustyline::Editor::<()>::with_config(config);

    let app_info = app_dirs2::AppInfo {
        name: "fractal",
        author: "Lorxu",
    };

    // Use the current directory if we can't get a app-specific one for some reason
    let mut history_file = app_dirs2::app_root(app_dirs2::AppDataType::UserData, &app_info)
        .unwrap_or_else(|_| {
            let p = std::path::PathBuf::from("./.fractal");
            if !p.exists() {
                std::fs::create_dir_all(&p).unwrap_or_else(|_| {
                    println!("Failed to create history directory in the current directory")
                });
            }
            p
        });
    history_file.push("repl_history");
    if history_file.exists() {
        rl.load_history(&history_file).unwrap();
    }

    loop {
        let s = if buf.is_empty() {
            rl.readline(">> ")
        } else {
            rl.readline(">| ")
        };
        if s.as_ref().map(|x| x.trim() == "exit").unwrap_or(true) {
            rl.save_history(&history_file)
                .unwrap_or_else(|e| println!("Failed to write history file, error {:?}", e));
            println!("Goodbye");
            break;
        }
        let s = s.unwrap();

        // If we can parse this line, do that;
        // If not, assume it's multiple lines and stop when they give us a blank line
        let result = if buf.is_empty() {
            match parse::parse_str(format!("<interactive:{}>", i), &s) {
                r @ Ok(_) => r,
                Err(_) => {
                    buf.push('\n');
                    buf.push_str(&s);
                    continue;
                }
            }
        } else if s.trim().is_empty() {
            parse::parse_str(format!("<interactive:{}>", i), buf)
        } else {
            buf.push('\n');
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
                                i.write().unwrap();
                            }
                        }
                    }
                }
            }
            Err(e) => e.write().unwrap(),
        }
    }
}

fn main() {
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
        match parse::parse_str(f, s) {
            Ok(result) => {
                let b = Node::new_raw(ast::Term::Block(result));
                if verbose {
                    println!("{}", b);
                }
                use pattern::HasTotal;
                match b.total(&mut env, false) {
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
                            i.write().unwrap();
                        }
                    }
                }
            }
            Err(e) => e.write().unwrap(),
        }
    } else {
        repl(verbose)
    }
}
