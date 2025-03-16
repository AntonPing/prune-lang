use super::*;

#[derive(Clone, Debug, PartialEq)]
pub struct Constr {
    pub prims: Vec<(Prim, Vec<UnifyTerm>)>,
}

impl Display for Constr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (prim, args) in self.prims.iter() {
            let args = args.iter().format(&", ");
            writeln!(f, "{prim:?}({args})")?;
        }
        Ok(())
    }
}
