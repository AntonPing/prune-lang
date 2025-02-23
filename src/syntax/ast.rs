use super::*;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Program {
    pub datas: Vec<DataDecl>,
    pub funcs: Vec<FuncDecl>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Declaration {
    Data(DataDecl),
    Func(FuncDecl),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct FuncDecl {
    pub name: Ident,
    pub pars: Vec<(Ident, LitType)>,
    pub res: LitType,
    pub body: Expr,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct DataDecl {
    pub name: Ident,
    pub cons: Vec<Constructor>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Constructor {
    pub name: Ident,
    pub flds: Vec<(Ident, LitType)>,
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
    Cons {
        name: Ident,
        flds: Vec<(Ident, Expr)>,
    },
    Fld {
        var: Ident,
        fld: Ident,
    },
    Match {
        expr: Box<Expr>,
        bind: Ident,
        brchs: Vec<(Ident, Expr)>,
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
