use super::*;
use crate::logic;
use crate::syntax::ast;

fn compile_type(typ: &ast::Type) -> TermType {
    match typ {
        ast::Type::Lit { lit, span: _ } => Term::Lit(*lit),
        ast::Type::Var { var, span: _ } => Term::Var(var.ident),
        ast::Type::Cons {
            cons,
            flds,
            span: _,
        } => {
            let flds = flds.iter().map(compile_type).collect();
            Term::Cons(OptCons::Some(cons.ident), flds)
        }
        ast::Type::Tuple { flds, span: _ } => {
            let flds: Vec<TermType> = flds.iter().map(compile_type).collect();
            Term::Cons(OptCons::None, flds)
        }
    }
}

fn compile_data_decl(data: &ast::DataDecl) -> DataDecl {
    let name = data.name.ident;
    let polys = data.polys.iter().map(|poly| poly.ident).collect();
    let cons = data.cons.iter().map(compile_constructor).collect();
    DataDecl { name, polys, cons }
}

fn compile_constructor(cons: &ast::Constructor) -> Constructor {
    let name = cons.name.ident;
    let flds = cons.flds.iter().map(compile_type).collect();
    Constructor { name, flds }
}

fn compile_query(query: &ast::QueryDecl) -> QueryDecl {
    QueryDecl {
        entry: query.entry.ident,
        params: query
            .params
            .iter()
            .map(|(param, _span)| compile_query_param(param))
            .collect(),
    }
}

fn compile_query_param(param: &ast::QueryParam) -> QueryParam {
    match param {
        ast::QueryParam::DepthStep(x) => QueryParam::DepthStep(*x),
        ast::QueryParam::DepthLimit(x) => QueryParam::DepthLimit(*x),
        ast::QueryParam::AnswerLimit(x) => QueryParam::AnswerLimit(*x),
        ast::QueryParam::AnswerPause(x) => QueryParam::AnswerPause(*x),
    }
}

pub fn compile_pass(prog: &ast::Program) -> Program {
    let datas: HashMap<Ident, DataDecl> = prog
        .datas
        .iter()
        .map(|data| (data.name.ident, compile_data_decl(data)))
        .collect();

    let preds: HashMap<Ident, PredDecl> = transform::logic_translation(&prog.funcs)
        .iter()
        .map(|(pred, pred_decl)| {
            let pred_decl = PredDecl {
                name: *pred,
                polys: pred_decl.polys.clone(),
                pars: pred_decl.pars.clone(),
                rules: logic::normalize::normalize_pred(pred_decl),
            };
            (*pred, pred_decl)
        })
        .collect();

    let querys = prog.querys.iter().map(compile_query).collect();

    let prog = Program {
        datas,
        preds,
        querys,
    };

    prog
}

#[test]
#[ignore = "just to see result"]
fn compile_pass_test() {
    let src: &'static str = r#"
datatype List[a] where
| Cons(a, List[a])
| Nil
end

function id[a](x: a) -> a
begin
    x
end

function append(xs: List[Int], x: Int) -> List[Int]
begin
    match xs with
    | Cons(head, tail) =>
        Cons(head, append(tail, id(x)))
    | Nil => Cons(x, Nil)
    end
end
"#;
    let (prog, errs) = crate::syntax::parser::parse_program(&src);
    assert!(errs.is_empty());

    let prog = super::compile::compile_pass(&prog);

    println!("{:#?}", prog);
}
