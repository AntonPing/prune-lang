use super::*;
use crate::logic::ast;
use crate::utils::unify::*;

struct Elaborator {
    val_ctx: HashMap<Ident, TypeId>,
    pred_ctx: HashMap<Ident, Vec<TypeId>>,
    cons_ctx: HashMap<Ident, (Vec<TypeId>, TypeId)>,
    data_ctx: HashMap<Ident, Vec<Ident>>,
    solver: Unifier<Ident, LitType, OptCons<Ident>>,
}

impl Elaborator {
    pub fn new() -> Elaborator {
        Elaborator {
            val_ctx: HashMap::new(),
            pred_ctx: HashMap::new(),
            cons_ctx: HashMap::new(),
            data_ctx: HashMap::new(),
            solver: Unifier::new(),
        }
    }

    fn fresh(&mut self) -> TypeId {
        TypeId::Var(Ident::fresh(&"t"))
    }

    fn unify(&mut self, typ1: &TypeId, typ2: &TypeId) {
        self.solver.unify(typ1, typ2).unwrap()
    }

    fn unify_many(&mut self, typs1: &[TypeId], typs2: &[TypeId]) {
        self.solver.unify_many(typs1, typs2).unwrap()
    }

    fn elab_var(&mut self, var: &Ident) -> TypeId {
        self.val_ctx.get(var).cloned().unwrap_or_else(|| {
            let typ = self.fresh();
            self.val_ctx.insert(*var, typ.clone());
            typ
        })
    }

    fn elab_atom(&mut self, atom: &AtomId) -> TypeId {
        match atom {
            Term::Var(var) => self
                .val_ctx
                .get(var)
                .cloned()
                .unwrap_or_else(|| self.fresh()),
            Term::Lit(lit) => TypeId::Lit(lit.get_typ()),
            Term::Cons(_cons, _flds) => unreachable!(),
        }
    }

    fn elab_type(typ: &TypeId) -> TypeId {
        match typ {
            Term::Var(_var) => panic!("generics not supported yet!"),
            Term::Lit(lit) => TypeId::Lit(*lit),
            Term::Cons(cons, flds) => {
                let flds = flds.iter().map(Self::elab_type).collect();
                TypeId::Cons(*cons, flds)
            }
        }
    }

    fn elab_goal(&mut self, goal: &ast::Goal) {
        match goal {
            ast::Goal::Lit(_) => {}
            ast::Goal::Eq(var, atom) => {
                let typ1 = self.elab_var(var);
                let typ2 = self.elab_atom(atom);
                self.unify(&typ1, &typ2);
            }
            ast::Goal::Cons(var, cons, flds) => {
                if let OptCons::Some(cons) = cons {
                    let (flds_ty, var_ty) = self.cons_ctx[cons].clone();
                    let var = self.elab_var(var);
                    let flds: Vec<_> = flds.iter().map(|fld| self.elab_atom(fld)).collect();
                    self.unify(&var, &var_ty);
                    self.unify_many(&flds, &flds_ty);
                } else {
                    let var = self.elab_var(var);
                    let flds: Vec<TypeId> = flds.iter().map(|fld| self.elab_atom(fld)).collect();
                    self.unify(&var, &TypeId::Cons(OptCons::None, flds));
                }
            }
            ast::Goal::Prim(prim, args) => {
                let pars: Vec<_> = prim.get_typ().iter().map(|lit| TypeId::Lit(*lit)).collect();
                let args: Vec<_> = args.iter().map(|arg| self.elab_atom(arg)).collect();
                self.unify_many(&pars, &args);
            }
            ast::Goal::And(goals) => {
                for goal in goals {
                    self.elab_goal(goal);
                }
            }
            ast::Goal::Or(goals) => {
                for goal in goals {
                    self.elab_goal(goal);
                }
            }
            ast::Goal::Call(pred, args) => {
                let pars = self.pred_ctx[pred].clone();
                let args: Vec<_> = args.iter().map(|arg| self.elab_atom(arg)).collect();
                self.unify_many(&pars, &args);
            }
        }
    }

    fn scan_data_decl_head(&mut self, data_decl: &ast::DataDecl) {
        let cons_names = data_decl.cons.iter().map(|cons| cons.name).collect();
        self.data_ctx.insert(data_decl.name, cons_names);

        for cons in data_decl.cons.iter() {
            let flds = cons.flds.iter().map(Self::elab_type).collect();
            self.cons_ctx.insert(
                cons.name,
                (
                    flds,
                    TypeId::Cons(OptCons::Some(data_decl.name), Vec::new()),
                ),
            );
        }
    }

    fn scan_pred_decl_head(&mut self, pred_decl: &ast::PredDecl) {
        let pars = pred_decl
            .pars
            .iter()
            .map(|par| self.elab_var(par))
            .collect();
        self.pred_ctx.insert(pred_decl.name, pars);
    }

    fn elab_pred_decl(&mut self, pred_decl: &ast::PredDecl) {
        let pars_ty = self.pred_ctx[&pred_decl.name].clone();
        for (par, par_ty) in pred_decl.pars.iter().zip(pars_ty) {
            self.val_ctx.insert(*par, par_ty);
        }

        for var in pred_decl.vars.iter() {
            let var_ty = self.elab_var(var);
            self.val_ctx.insert(*var, var_ty);
        }
        self.elab_goal(&pred_decl.goal);
    }

    fn elab_prog(&mut self, prog: &ast::Program) {
        for data_decl in prog.datas.values() {
            self.scan_data_decl_head(data_decl);
        }

        for pred_decl in prog.preds.values() {
            self.scan_pred_decl_head(pred_decl);
        }

        for pred_decl in prog.preds.values() {
            self.elab_pred_decl(pred_decl);
        }
    }
}

pub fn elab_pass(prog: &ast::Program) -> HashMap<Ident, TypeId> {
    let mut pass = Elaborator::new();
    pass.elab_prog(prog);
    let sol = pass.solver;
    pass.val_ctx
        .iter()
        .map(|(k, v)| (*k, sol.merge(v)))
        .collect()
}

#[test]
#[ignore = "just to see result"]
fn check_test() {
    let src: &'static str = r#"
datatype IntList where
| Cons(Int, IntList)
| Nil
end

function append(xs: IntList, x: Int) -> IntList
begin
    match xs with
    | Cons(head, tail) => Cons(head, append(tail, x))
    | Nil => Cons(x, Nil)
    end
end

function is_elem(xs: IntList, x: Int) -> Bool
begin
    match xs with
    | Cons(head, tail) => if head == x then true else is_elem(tail, x) 
    | Nil => false
    end
end

function is_elem_after_append(xs: IntList, x: Int)
begin
    guard !is_elem(append(xs, x), x);
end

query is_elem_after_append(depth_step=5, depth_limit=50, answer_limit=1)
"#;
    let (mut prog, errs) = crate::syntax::parser::parse_program(&src);
    assert!(errs.is_empty());

    let errs = crate::tych::rename::rename_pass(&mut prog);
    assert!(errs.is_empty());

    let errs = crate::tych::check::check_pass(&prog);
    assert!(errs.is_empty());

    let prog = crate::logic::transform::logic_translation(&prog);
    println!("{:#?}", prog);

    let map = elab_pass(&prog);
    println!("{:?}", map);
}
