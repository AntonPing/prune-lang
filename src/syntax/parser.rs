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
| Some { value: Int }
| None
end
"#;
    assert!(parser::DataDeclParser::new().parse(d1).is_ok());
}

#[test]
fn match_syntax_parser_test() {
    let p1: &'static str = r#"
datatype IntList where
| Cons { head: Int, tail: Int }
| Nil
end

function append(xs: Int, x: Int) -> Int
begin
    match xs as lst with
    | Cons => Cons { head: lst.head, tail: append(lst.tail, x) }
    | Nil => Cons { head: x, tail: Nil }
    end
end
"#;
    assert!(parser::ProgramParser::new().parse(p1).is_ok());
}
