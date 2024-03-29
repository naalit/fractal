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
    var f = fun var x:num => x + x
    f 2
"#);
    assert_match!(@ok r, Total::Lit(Literal::Int(4)));
    let r = run(r#"do
    var f = fun var x:num => x + x
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
    var f = fun var x:num => x + x
    var g = fun var y:num => f (f y)
    g 3
"#);
    assert_match!(@ok r, Total::Lit(Literal::Int(12)));
}

#[test]
fn test_do() {
    let r = run(r#"do
    var x = 3
    var y = do
        var x = 4
        x + 2 # 6
    x + y # 9
"#);
    assert_match!(@ok r, Total::Lit(Literal::Int(9)));
}

#[test]
fn test_dot() {
    let r = run("2 + 3");
    assert_match!(@ok r, Total::Lit(Literal::Int(5)));
    let r = run(r#"do
    var f = fun var x:num, var y:num => y - x
    var minus_two = 2.f
    minus_two 3
"#);
    assert_match!(@ok r, Total::Lit(Literal::Int(1)));
    let r = run(r#"do
    var f = fun
        3, 3 => 1
        4, 5 => 2
        9, 10 => 9000
        num, num => 180
    (3 f 3) + # 1
        (4.f 5) + # 2
            (2 f 3) # 180
"#);
    assert_match!(@ok r, Total::Lit(Literal::Int(183)));
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
