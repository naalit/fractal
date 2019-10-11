//! I could include more tests in modules themselves, but it's easier to write them in Fractal than in strange Rust data structures

use fractal::{assert_match, run, Literal, RunError::*, Total};

#[test]
fn it_works() {
    let r = run("3");
    assert_match!(r, Ok(_));
}

#[test]
#[should_panic]
fn assert_match_works() {
    assert_match!(1, 2);
}

#[test]
fn test_inter() {
    let r = run("var x : num = ()");
    assert_match!(r, Err(MatchError(_)));
    let r = run("var x : num = 243");
    assert_match!(r, Ok(_));
}

#[test]
fn test_app() {
    let r = run(r#"do
    var f = fun var x:num => + x, x
    f 2
"#);
    assert_match!(@ok r, Total::Lit(Literal::Int(4)));
    let r = run(r#"do
    var f = fun var x:num => + x, x
    fun var g:num => f g
"#);
    if let Ok(x) = &r {
        if let Total::Fun(v) = &**x {
            // we don't know what 'g' is, so we can't apply it
            if let Total::App(_, _, _) = &*v[0].1 {
                return;
            }
        }
    }
    panic!("{:?} didn't apply correctly", r);
}

#[test]
/// Makes sure that functions that can't be fully applied immediately propagate their return types correctly
fn test_ret() {
    let r = run(r#"do
    var f = fun var x:num => + x, x
    var g = fun var y:num => f (f y)
    g 3
"#);
    assert_match!(@ok r, Total::Lit(Literal::Int(12)));
}

#[test]
fn test_union() {
    let r = run(r#"do
    var f = fun 2 | 3 => ()
    f 3
"#);
    assert_match!(r, Ok(_));
    let r = run(r#"do
    var f = fun 2 | 3 => ()
    f (2 | 3)
"#);
    assert_match!(r, Err(MatchError(_)));
}
