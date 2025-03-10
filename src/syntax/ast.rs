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
    pub pars: Vec<(Ident, Type)>,
    pub res: Type,
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
    pub flds: Vec<Type>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Type {
    Lit(LitType),
    Data(Ident),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Pattern {
    pub name: Ident,
    pub flds: Vec<Ident>,
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
        flds: Vec<Expr>,
    },
    Match {
        expr: Box<Expr>,
        brchs: Vec<(Pattern, Expr)>,
    },
    Let {
        bind: Ident,
        expr: Box<Expr>,
        cont: Box<Expr>,
    },
    App {
        func: Ident,
        args: Vec<Expr>,
    },
    Assert {
        expr: Box<Expr>,
        cont: Box<Expr>,
    },
}
