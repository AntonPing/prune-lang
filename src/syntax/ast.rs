use super::*;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Program {
    pub funcs: Vec<FuncDecl>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct FuncDecl {
    pub name: Ident,
    pub pars: Vec<(Ident, LitType)>,
    pub res: LitType,
    pub body: Expr,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Expr {
    Lit {
        lit: LitVal,
    },
    Var {
        var: Ident,
    },
    Prim {
        prim: Prim,
        args: Vec<Expr>,
    },
    Let {
        bind: Ident,
        body: Box<Expr>,
        cont: Box<Expr>,
    },
    App {
        func: Ident,
        args: Vec<Expr>,
    },
}
