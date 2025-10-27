use super::*;

#[derive(Clone, Debug, PartialEq)]
pub struct Program {
    pub datas: Vec<DataDecl>,
    pub funcs: Vec<FuncDecl>,
    pub preds: Vec<PredDecl>,
    pub querys: Vec<QueryDecl>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Declaration {
    Data(DataDecl),
    Func(FuncDecl),
    Pred(PredDecl),
    Query(QueryDecl),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Var {
    pub ident: Ident,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct DataDecl {
    pub name: Var,
    pub cons: Vec<Constructor>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Constructor {
    pub name: Var,
    pub flds: Vec<Type>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Lit {
        lit: LitType,
        span: Span,
    },
    Cons {
        cons: Var,
        flds: Vec<Type>,
        span: Span,
    },
    Tuple {
        flds: Vec<Type>,
        span: Span,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct FuncDecl {
    pub name: Var,
    pub pars: Vec<(Var, Type)>,
    pub res: Type,
    pub body: Expr,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Pattern {
    Lit {
        lit: LitVal,
        span: Span,
    },
    Var {
        var: Var,
        span: Span,
    },
    Cons {
        cons: Var,
        flds: Vec<Pattern>,
        span: Span,
    },
    Tuple {
        flds: Vec<Pattern>,
        span: Span,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Lit {
        lit: LitVal,
        span: Span,
    },
    Var {
        var: Var,
        span: Span,
    },
    Prim {
        prim: Prim,
        args: Vec<Expr>,
        span: Span,
    },
    Cons {
        cons: Var,
        flds: Vec<Expr>,
        span: Span,
    },
    Tuple {
        flds: Vec<Expr>,
        span: Span,
    },
    Match {
        expr: Box<Expr>,
        brchs: Vec<(Pattern, Expr)>,
        span: Span,
    },
    Let {
        patn: Pattern,
        expr: Box<Expr>,
        cont: Box<Expr>,
        span: Span,
    },
    App {
        func: Var,
        args: Vec<Expr>,
        span: Span,
    },
    Ifte {
        cond: Box<Expr>,
        then: Box<Expr>,
        els: Box<Expr>,
        span: Span,
    },
    Cond {
        brchs: Vec<(Expr, Expr)>,
        span: Span,
    },
    Alter {
        brchs: Vec<Expr>,
        span: Span,
    },
    Fresh {
        vars: Vec<Var>,
        cont: Box<Expr>,
        span: Span,
    },
    Guard {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        cont: Box<Expr>,
        span: Span,
    },
    Undefined {
        span: Span,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct PredDecl {
    pub name: Var,
    pub pars: Vec<(Var, Type)>,
    pub body: Goal,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Goal {
    Fresh {
        vars: Vec<Var>,
        body: Box<Goal>,
        span: Span,
    },
    Eq {
        lhs: Expr,
        rhs: Expr,
        span: Span,
    },
    Pred {
        pred: Var,
        args: Vec<Expr>,
        span: Span,
    },
    And {
        goals: Vec<Goal>,
        span: Span,
    },
    Or {
        goals: Vec<Goal>,
        span: Span,
    },
    Lit {
        val: bool,
        span: Span,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct QueryDecl {
    pub entry: Var,
    pub params: Vec<(QueryParam, Span)>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum QueryParam {
    DepthStep(usize),
    DepthLimit(usize),
    AnswerLimit(usize),
    AnswerPause(bool),
}
