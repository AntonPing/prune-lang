use super::*;

use crate::utils::unify::*;

#[derive(Clone, Debug)]
struct PredTyScm {
    polys: Vec<Ident>,
    pars: Vec<TermType>,
}

#[derive(Clone, Debug)]
struct ConsTyScm {
    polys: Vec<Ident>,
    flds: Vec<TermType>,
    res: TermType,
}

#[allow(unused)]
#[derive(Clone, Debug)]
struct DataTyScm {
    // todo: use this to check type intro. rules
    polys: Vec<Ident>,
}

struct Elaborator {
    val_ctx: HashMap<Ident, TermType>,
    pred_ctx: HashMap<Ident, PredTyScm>,
    cons_ctx: HashMap<Ident, ConsTyScm>,
    data_ctx: HashMap<Ident, DataTyScm>,
    unifier: Unifier<Ident, LitType, OptCons<Ident>>,
}

impl Elaborator {
    pub fn new() -> Elaborator {
        Elaborator {
            val_ctx: HashMap::new(),
            pred_ctx: HashMap::new(),
            cons_ctx: HashMap::new(),
            data_ctx: HashMap::new(),
            unifier: Unifier::new(),
        }
    }

    fn scan_data_ty_scm(&mut self, data_decl: &DataDecl) {
        for poly in data_decl.polys.iter() {
            self.unifier.fresh(*poly);
        }
        let data_scm = DataTyScm {
            polys: data_decl.polys.clone(),
        };
        self.data_ctx.insert(data_decl.name, data_scm);
    }

    fn scan_cons_ty_scm(&mut self, data_decl: &DataDecl) {
        let res = TermType::Cons(
            OptCons::Some(data_decl.name),
            data_decl
                .polys
                .iter()
                .map(|poly| TermType::Var(*poly))
                .collect(),
        );

        for cons in &data_decl.cons {
            let cons_typ = ConsTyScm {
                polys: data_decl.polys.clone(),
                flds: cons.flds.clone(),
                res: res.clone(),
            };
            self.cons_ctx.insert(cons.name, cons_typ);
        }
    }

    fn scan_pred_ty_scm(&mut self, pred_decl: &PredDecl) {
        for poly in pred_decl.polys.iter() {
            self.unifier.fresh(*poly);
        }

        let polys = pred_decl.polys.clone();

        let pars = pred_decl
            .pars
            .iter()
            .map(|(_par, typ)| typ.clone())
            .collect();

        let pred_scm = PredTyScm { polys, pars };
        self.pred_ctx.insert(pred_decl.name, pred_scm);
    }

    fn elab_term(&mut self, term: &TermVal) -> TermType {
        match term {
            Term::Var(var) => self.val_ctx[var].clone(),
            Term::Lit(lit) => TermType::Lit(lit.get_typ()),
            Term::Cons(cons, flds) => {
                let flds: Vec<_> = flds.iter().map(|fld| self.elab_term(fld)).collect();
                if let OptCons::Some(cons) = cons {
                    // instantiate constructor type scheme
                    let cons_scm = &self.cons_ctx[cons];

                    let inst_map: HashMap<Ident, TermType> = cons_scm
                        .polys
                        .iter()
                        .map(|poly| (*poly, Term::Var(poly.uniquify())))
                        .collect();

                    let inst_flds: Vec<_> = cons_scm
                        .flds
                        .iter()
                        .map(|fld| fld.substitute(&inst_map))
                        .collect();

                    let inst_res = cons_scm.res.substitute(&inst_map);

                    self.unifier.unify_many(&inst_flds, &flds).unwrap();

                    inst_res
                } else {
                    TermType::Cons(OptCons::None, flds)
                }
            }
        }
    }

    fn elab_prim(&mut self, prim: Prim, args: &[AtomVal]) {
        let pars: Vec<TermType> = prim
            .get_typ()
            .into_iter()
            .map(|typ| Term::Lit(typ))
            .collect();

        let args: Vec<TermType> = args
            .iter()
            .map(|arg| self.elab_term(&arg.to_term()))
            .collect();

        self.unifier.unify_many(&pars, &args).unwrap();
    }

    fn elab_call(&mut self, pred: Ident, polys: &Vec<TermType>, args: &Vec<TermVal>) {
        let args: Vec<_> = args.iter().map(|arg| self.elab_term(arg)).collect();

        // instantiate predicate type scheme
        let pred_scm = &self.pred_ctx[&pred];

        let inst_map: HashMap<Ident, TermType> = pred_scm
            .polys
            .iter()
            .cloned()
            .zip(polys.iter().cloned())
            .collect();

        let inst_pars: Vec<_> = pred_scm
            .pars
            .iter()
            .map(|par| par.substitute(&inst_map))
            .collect();

        self.unifier.unify_many(&inst_pars, &args).unwrap();
    }

    fn elab_rule(&mut self, rule: &Rule) {
        for (var, typ) in rule.vars.iter() {
            self.val_ctx.insert(*var, typ.clone());
        }

        for term in rule.head.iter() {
            self.elab_term(term);
        }

        for (pred, polys, args) in rule.calls.iter() {
            self.elab_call(*pred, polys, args);
        }

        for (prim, args) in rule.prims.iter() {
            self.elab_prim(*prim, args);
        }
    }

    fn elab_pred_decl(&mut self, pred_decl: &mut PredDecl) {
        for (par, typ) in pred_decl.pars.iter() {
            self.val_ctx.insert(*par, typ.clone());
        }

        for rule in pred_decl.rules.iter() {
            self.elab_rule(rule);
        }
    }

    fn merge_pred_decl(&self, pred_decl: &mut PredDecl) {
        for rule in pred_decl.rules.iter_mut() {
            self.merge_rule(rule);
        }
    }

    fn merge_rule(&self, rule: &mut Rule) {
        for (_var, typ) in rule.vars.iter_mut() {
            *typ = self.unifier.merge(typ);
        }

        for (_pred, polys, _args) in rule.calls.iter_mut() {
            for poly in polys {
                *poly = self.unifier.merge(poly);
            }
        }
    }

    fn elab_prog(&mut self, prog: &mut Program) {
        for (_, data_decl) in prog.datas.iter() {
            self.scan_data_ty_scm(data_decl);
        }

        for (_, data_decl) in prog.datas.iter() {
            self.scan_cons_ty_scm(data_decl);
        }

        for (_, pred_decl) in prog.preds.iter() {
            self.scan_pred_ty_scm(pred_decl);
        }

        for (_, pred_decl) in prog.preds.iter_mut() {
            self.elab_pred_decl(pred_decl);
        }

        for (_, pred_decl) in prog.preds.iter_mut() {
            self.merge_pred_decl(pred_decl);
        }
    }
}

pub fn elab_pass(prog: &mut Program) {
    let mut pass = Elaborator::new();
    pass.elab_prog(prog);
}

#[test]
#[ignore = "just to see result"]
fn elab_pass_test() {
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

    let mut prog = super::compile::compile_pass(&prog);

    println!("{:#?}", prog);

    super::elab::elab_pass(&mut prog);

    println!("{:#?}", prog);
}
