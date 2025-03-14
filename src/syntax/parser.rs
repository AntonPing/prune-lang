use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub parser, "/syntax/grammar.rs"); // synthesized by LALRPOP

#[test]
fn func_decl_parser_test() {
    let f1: &'static str = r#"
function f1(x: Int, y: Int) -> Int
begin
    let c = 42;
    let v = x;
    let z = @iadd(v, y);
    let r = f2(x, y, z);
    r
end
"#;
    assert!(parser::FuncDeclParser::new().parse(f1).is_ok());
}

#[test]
fn data_decl_parser_test() {
    let d1: &'static str = r#"
datatype OptInt where
| Some(Int)
| None
end
"#;
    assert!(parser::DataDeclParser::new().parse(d1).is_ok());
}

#[test]
fn match_syntax_parser_test_1() {
    let p1: &'static str = r#"
datatype IntList where
| Cons(Int, IntList)
| Nil
end

function append(xs: IntList, x: Int) -> Int
begin
    match xs with
    | Cons(head, tail) => Cons(head, append(tail, x))
    | Nil => Cons(x, Nil)
    end
end
"#;
    assert!(parser::ProgramParser::new().parse(p1).is_ok());
}

#[test]
fn match_syntax_parser_test_2() {
    let p1: &'static str = r#"
datatype AVLTree where
| Node(AVLTree, Int, AVLTree)
| Empty
end

function insert(tree: AVLTree, x: Int) -> AVLTree
begin
    match tree with
    | Node(left, y, right) => 
        if @icmplt(x, y) then
            Node(insert(left, x), y, right)
        else if @icmpgt(x, y) then
            Node(left, y, insert(right, x))
        else tree
    | Empty => Node(Empty, x, Empty)
    end
end
"#;
    assert!(parser::ProgramParser::new().parse(p1).is_ok());
}
