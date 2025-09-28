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

#[derive(Clone, Debug, PartialEq)]
pub struct DataDecl {
    pub name: Ident,
    pub cons: Vec<Constructor>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Constructor {
    pub name: Ident,
    pub flds: Vec<Type>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Lit(LitType),
    Data(Ident),
}

#[derive(Clone, Debug, PartialEq)]
pub struct FuncDecl {
    pub name: Ident,
    pub pars: Vec<(Ident, Type)>,
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
        var: Ident,
        span: Span,
    },
    Cons {
        cons: Ident,
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
        var: Ident,
        span: Span,
    },
    Prim {
        prim: Prim,
        args: Vec<Expr>,
        span: Span,
    },
    Cons {
        cons: Ident,
        flds: Vec<Expr>,
        span: Span,
    },
    Match {
        expr: Box<Expr>,
        brchs: Vec<(Pattern, Expr)>,
        span: Span,
    },
    Let {
        bind: Ident,
        expr: Box<Expr>,
        cont: Box<Expr>,
        span: Span,
    },
    App {
        func: Ident,
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
    Guard {
        goal: Box<Goal>,
        cont: Box<Expr>,
        span: Span,
    },
    GoalFail {
        span: Span,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct PredDecl {
    pub name: Ident,
    pub pars: Vec<(Ident, Type)>,
    pub body: Goal,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Goal {
    Fresh {
        vars: Vec<Ident>,
        body: Box<Goal>,
        span: Span,
    },
    Eq {
        lhs: Expr,
        rhs: Expr,
        span: Span,
    },
    Pred {
        pred: Ident,
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
    pub entry: Ident,
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
