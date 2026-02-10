use super::*;

pub trait PrimSolver {
    fn check_sat(
        &mut self,
        prims: &[(Prim, Vec<AtomVal<IdentCtx>>)],
    ) -> Option<HashMap<IdentCtx, LitVal>>;
}

pub fn infer_type(prims: &[(Prim, Vec<AtomVal<IdentCtx>>)]) -> HashMap<IdentCtx, LitType> {
    let mut map = HashMap::new();

    for (prim, args) in prims.iter() {
        for (arg, typ) in args.iter().zip(prim.get_typ().iter()) {
            match arg {
                Term::Var(var) => {
                    if let Some(res) = map.get(var) {
                        assert_eq!(*res, *typ);
                    } else {
                        map.insert(*var, *typ);
                    }
                }
                Term::Lit(lit) => {
                    assert_eq!(lit.get_typ(), *typ);
                }
                Term::Cons(_, _) => unreachable!(),
            }
        }
    }

    map
}
