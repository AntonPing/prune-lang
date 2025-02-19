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
fn expr_parser_test() {
    assert!(parser::ExprParser::new().parse("22").is_ok());
    assert!(parser::ExprParser::new().parse("(22,)").is_ok());
    assert!(parser::ExprParser::new().parse("(22,33)").is_ok());
    assert!(parser::ExprParser::new().parse("((22)").is_err());
}
