use super::*;

use crate::syntax::ast::Type;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UnifyType {
    Lit(LitType),
    Var(Ident),
    Cons(Option<Ident>, Vec<UnifyType>),
    Cell(usize),
}

impl From<&Type> for UnifyType {
    fn from(value: &Type) -> Self {
        match value {
            Type::Lit { lit, span: _ } => UnifyType::Lit(*lit),
            Type::Cons {
                cons,
                flds,
                span: _,
            } => {
                let flds = flds.iter().map(|fld| fld.into()).collect();
                UnifyType::Cons(Some(cons.ident), flds)
            }
            Type::Tuple { flds, span: _ } => {
                let flds = flds.iter().map(|fld| fld.into()).collect();
                UnifyType::Cons(None, flds)
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UnifyError {
    VecDiffLen(Vec<UnifyType>, Vec<UnifyType>),
    CannotUnify(UnifyType, UnifyType),
    OccurCheckFailed(usize, UnifyType),
}

use crate::driver::diagnostic::Diagnostic;
impl From<UnifyError> for Diagnostic {
    // todo: better error message
    fn from(val: UnifyError) -> Self {
        match val {
            UnifyError::VecDiffLen(_, _) => Diagnostic::error("VecDiffLen!".to_string()),
            UnifyError::CannotUnify(lhs, rhs) => {
                Diagnostic::error(format!("CannotUnify {:?} and {:?}!", lhs, rhs))
            }
            UnifyError::OccurCheckFailed(_, _) => {
                Diagnostic::error("OccurCheckFailed!".to_string())
            }
        }
    }
}

type UnifyResult = Result<(), UnifyError>;

pub struct UnifySolver {
    arena: Vec<Option<UnifyType>>,
}

impl Default for UnifySolver {
    fn default() -> Self {
        Self::new()
    }
}

impl UnifySolver {
    pub fn new() -> UnifySolver {
        UnifySolver { arena: Vec::new() }
    }

    pub fn new_cell(&mut self) -> usize {
        self.arena.push(None);
        self.arena.len() - 1
    }

    fn deref(&self, typ: &UnifyType) -> UnifyType {
        match typ {
            UnifyType::Lit(_) => typ.clone(),
            UnifyType::Var(_) => typ.clone(),
            UnifyType::Cons(_, _) => typ.clone(),
            UnifyType::Cell(cell) => {
                if let Some(typ2) = &self.arena[*cell] {
                    self.deref(typ2)
                } else {
                    typ.clone()
                }
            }
        }
    }

    pub fn unify_many(&mut self, typs1: &[UnifyType], typs2: &[UnifyType]) -> UnifyResult {
        if typs1.len() != typs2.len() {
            Err(UnifyError::VecDiffLen(typs1.to_vec(), typs2.to_vec()))
        } else {
            for (typ1, typ2) in typs1.iter().zip(typs2.iter()) {
                self.unify(typ1, typ2)?;
            }
            Ok(())
        }
    }

    pub fn unify(&mut self, typ1: &UnifyType, typ2: &UnifyType) -> UnifyResult {
        let typ1 = self.deref(typ1);
        let typ2 = self.deref(typ2);
        match (typ1, typ2) {
            (UnifyType::Lit(lit1), UnifyType::Lit(lit2)) if lit1 == lit2 => Ok(()),
            (UnifyType::Var(ident1), UnifyType::Var(ident2)) if ident1 == ident2 => Ok(()),
            (UnifyType::Cons(cons1, args1), UnifyType::Cons(cons2, args2)) if cons1 == cons2 => {
                self.unify_many(&args1, &args2)?;
                Ok(())
            }
            (UnifyType::Cell(cell1), UnifyType::Cell(cell2)) => {
                if cell1 == cell2 {
                    Ok(())
                } else {
                    self.arena[cell1] = Some(UnifyType::Cell(cell2));
                    Ok(())
                }
            }
            (UnifyType::Cell(cell), typ) | (typ, UnifyType::Cell(cell)) => {
                if self.occur_check(cell, &typ) {
                    Err(UnifyError::OccurCheckFailed(cell, typ.clone()))
                } else {
                    self.arena[cell] = Some(typ.clone());
                    Ok(())
                }
            }
            (typ1, typ2) => Err(UnifyError::CannotUnify(typ1.clone(), typ2.clone())),
        }
    }

    fn occur_check(&self, cell: usize, typ: &UnifyType) -> bool {
        match typ {
            UnifyType::Lit(_) => false,
            UnifyType::Var(_) => false,
            UnifyType::Cons(_, args) => args.iter().any(|arg| self.occur_check(cell, arg)),
            UnifyType::Cell(cell2) if cell == *cell2 => true,
            UnifyType::Cell(cell2) => {
                if let Some(typ2) = &self.arena[*cell2] {
                    self.occur_check(cell, typ2)
                } else {
                    false
                }
            }
        }
    }

    pub fn merge(&self, typ: &UnifyType) -> TypeId {
        match typ {
            UnifyType::Lit(lit) => Term::Lit(*lit),
            UnifyType::Var(var) => Term::Var(*var),
            UnifyType::Cons(cons, args) => {
                let args = args.iter().map(|arg| self.merge(arg)).collect();
                Term::Cons(*cons, args)
            }
            UnifyType::Cell(cell) => {
                if self.arena[*cell].is_some() {
                    self.merge(self.arena[*cell].as_ref().unwrap())
                } else {
                    let var = Ident::dummy(&format!("?{cell}"));
                    Term::Var(var)
                }
            }
        }
    }
}

pub fn substitute(map: &HashMap<Ident, usize>, typ: &UnifyType) -> UnifyType {
    match typ {
        UnifyType::Lit(lit) => UnifyType::Lit(*lit),
        UnifyType::Var(ident) => {
            if let Some(cell) = map.get(ident) {
                UnifyType::Cell(*cell)
            } else {
                UnifyType::Var(*ident)
            }
        }
        UnifyType::Cons(cons, args) => {
            let args = args.iter().map(|arg| substitute(map, arg)).collect();
            UnifyType::Cons(*cons, args)
        }
        UnifyType::Cell(cell) => UnifyType::Cell(*cell),
    }
}
