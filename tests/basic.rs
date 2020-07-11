use simple_ii_lib::interpreter::Interpreter;
#[test]
fn basic_arithmetic() {
    let mut i = Interpreter::new();
    assert_eq!(i.input("1 + 1"), Ok(Some(2.0)));
    assert_eq!(i.input("2 - 1"), Ok(Some(1.0)));
    assert_eq!(i.input("2 * 3"), Ok(Some(6.0)));
    assert_eq!(i.input("8 / 4"), Ok(Some(2.0)));
    assert_eq!(i.input("7 % 4"), Ok(Some(3.0)));
}

#[test]
fn variables() {
    let mut i = Interpreter::new();
    assert_eq!(i.input("x = 1"), Ok(Some(1.0)));
    assert_eq!(i.input("x"), Ok(Some(1.0)));
    assert_eq!(i.input("x + 3"), Ok(Some(4.0)));
    assert!(i.input("y").is_err());
}

#[test]
fn functions() {
    let mut i = Interpreter::new();
    assert_eq!(i.input("fn avg x y => (x + y) / 2"), Ok(None));
    assert_eq!(i.input("avg 4 2"), Ok(Some(3.0)));
    assert!(i.input("avg 7").is_err());
    assert!(i.input("avg 7 2 4").is_err());
}

#[test]
fn conflicts() {
    let mut i = Interpreter::new();
    assert_eq!(i.input("x = 1"), Ok(Some(1.0)));
    assert_eq!(i.input("fn avg x y => (x + y) / 2"), Ok(None));
    assert!(i.input("fn x => 0").is_err());
    assert!(i.input("avg = 5").is_err());
}

#[test]
fn op_order() {
    let mut i = Interpreter::new();
    assert_eq!(i.input("4 + 2 * 3"), Ok(Some(10.0)))
}

#[test]
fn chained_functions() {
    let mut i = Interpreter::new();
    assert_eq!(i.input("fn f x y => x"), Ok(None));
    assert_eq!(i.input("fn g x y z => x"), Ok(None));
    assert_eq!(i.input("x = 1"), Ok(Some(1.0)));
    assert_eq!(i.input("y = 2"), Ok(Some(2.0)));
    assert!(i.input("f y x x").is_err());
    assert_eq!(i.input("g g 1 2 3 f 4 5 f 6 7"), Ok(Some(1.0)));
    assert!(i.input("g g 1 2 3 f 4 5 f 6").is_err());
    assert_eq!(i.input("fn id x => x"), Ok(None));
    assert_eq!(i.input("fn add x y => x + y"), Ok(None));
    assert_eq!(i.input("add id 1 id 2"), Ok(Some(3.0)));
    assert!(i.input("add id 1 id 2 3").is_err());
}
