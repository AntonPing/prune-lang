use super::*;

use crate::utils::prim::Prim;
use crate::utils::unify::*;

#[derive(Clone, Debug)]
struct PredTyScm {
    polys: Vec<Ident>,
    pars: Vec<TypeId>,
}

#[derive(Clone, Debug)]
struct ConsTyScm {
    polys: Vec<Ident>,
    flds: Vec<TypeId>,
    res: TypeId,
}

#[allow(unused)]
#[derive(Clone, Debug)]
struct DataTyScm {
    // todo: use this to check type intro. rules
    polys: Vec<Ident>,
}

struct Elaborator {
    val_ctx: HashMap<Ident, TypeId>,
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

    fn elab_term(&mut self, term: &TermId) -> TypeId {
        match term {
            Term::Var(var) => self.val_ctx[var].clone(),
            Term::Lit(lit) => TypeId::Lit(lit.get_typ()),
            Term::Cons(cons, flds) => {
                let flds: Vec<_> = flds.iter().map(|fld| self.elab_term(fld)).collect();
                if let OptCons::Some(cons) = cons {
                    // instantiate constructor type scheme
                    let cons_scm = &self.cons_ctx[cons];

                    let inst_map: HashMap<Ident, TypeId> = cons_scm
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
                    TypeId::Cons(OptCons::None, flds)
                }
            }
        }
    }

    fn elab_prim(&mut self, prim: Prim, args: &[AtomId]) {
        let args: Vec<_> = args
            .iter()
            .map(|arg| self.elab_term(&arg.to_term()))
            .collect();

        match prim {
            Prim::IAdd | Prim::ISub | Prim::IMul | Prim::IDiv | Prim::IRem => {
                self.unifier
                    .unify_many(
                        &[
                            TypeId::Lit(LitType::TyInt),
                            TypeId::Lit(LitType::TyInt),
                            TypeId::Lit(LitType::TyInt),
                        ],
                        &args,
                    )
                    .unwrap();
            }
            Prim::INeg => {
                self.unifier
                    .unify_many(
                        &[TypeId::Lit(LitType::TyInt), TypeId::Lit(LitType::TyInt)],
                        &args,
                    )
                    .unwrap();
            }
            Prim::ICmp(_) => {
                self.unifier
                    .unify_many(
                        &[
                            TypeId::Lit(LitType::TyInt),
                            TypeId::Lit(LitType::TyInt),
                            TypeId::Lit(LitType::TyBool),
                        ],
                        &args,
                    )
                    .unwrap();
            }
            Prim::BAnd | Prim::BOr => {
                self.unifier
                    .unify_many(
                        &[
                            TypeId::Lit(LitType::TyBool),
                            TypeId::Lit(LitType::TyBool),
                            TypeId::Lit(LitType::TyBool),
                        ],
                        &args,
                    )
                    .unwrap();
            }
            Prim::BNot => {
                self.unifier
                    .unify_many(
                        &[TypeId::Lit(LitType::TyBool), TypeId::Lit(LitType::TyBool)],
                        &args,
                    )
                    .unwrap();
            }
        }
    }

    fn elab_goal(&mut self, goal: &mut Goal) {
        match goal {
            Goal::Lit(_) => {}
            Goal::Eq(lhs, rhs) => {
                let lhs = self.elab_term(lhs);
                let rhs = self.elab_term(rhs);
                self.unifier.unify(&lhs, &rhs).unwrap();
            }
            Goal::Prim(prim, args) => {
                self.elab_prim(*prim, args);
            }
            Goal::And(goals) => {
                for goal in goals {
                    self.elab_goal(goal);
                }
            }
            Goal::Or(goals) => {
                for goal in goals {
                    self.elab_goal(goal);
                }
            }
            Goal::Call(pred, polys, args) => {
                let args: Vec<_> = args.iter().map(|arg| self.elab_term(arg)).collect();

                // instantiate predicate type scheme
                let pred_scm = &self.pred_ctx[pred];

                let inst_map: HashMap<Ident, TypeId> = pred_scm
                    .polys
                    .iter()
                    .map(|poly| (*poly, Term::Var(poly.uniquify())))
                    .collect();

                let inst_polys: Vec<_> = pred_scm
                    .polys
                    .iter()
                    .map(|poly| inst_map[poly].clone())
                    .collect();

                let inst_pars: Vec<_> = pred_scm
                    .pars
                    .iter()
                    .map(|par| par.substitute(&inst_map))
                    .collect();

                *polys = inst_polys;

                self.unifier.unify_many(&inst_pars, &args).unwrap();
            }
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
        let res = TypeId::Cons(
            OptCons::Some(data_decl.name),
            data_decl
                .polys
                .iter()
                .map(|poly| TypeId::Var(*poly))
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

    fn elab_pred_decl(&mut self, pred_decl: &mut PredDecl) {
        for (par, typ) in pred_decl.pars.iter() {
            self.val_ctx.insert(*par, typ.clone());
        }
        for (var, typ) in pred_decl.vars.iter() {
            self.val_ctx.insert(*var, typ.clone());
        }
        self.elab_goal(&mut pred_decl.goal);
    }

    fn merge_pred_decl(&self, pred_decl: &mut PredDecl) {
        for (_var, typ) in pred_decl.vars.iter_mut() {
            *typ = self.unifier.merge(typ);
        }

        self.merge_goal(&mut pred_decl.goal);
    }

    fn merge_goal(&self, goal: &mut Goal) {
        match goal {
            Goal::Lit(_) => {}
            Goal::Eq(_, _) => {}
            Goal::Prim(_, _) => {}
            Goal::And(goals) => {
                for goal in goals {
                    self.merge_goal(goal);
                }
            }
            Goal::Or(goals) => {
                for goal in goals {
                    self.merge_goal(goal);
                }
            }
            Goal::Call(_pred, polys, _args) => {
                for poly in polys {
                    *poly = self.unifier.merge(poly);
                }
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
fn prog_to_pred_test() {
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

    let mut prog = super::transform::logic_translation(&prog);
    println!("{:#?}", prog);

    elab_pass(&mut prog);
    println!("{:#?}", prog);
}
