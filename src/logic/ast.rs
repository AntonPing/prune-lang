use super::*;

use itertools::Itertools;
use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub struct Program {
    pub datas: HashMap<Ident, DataDecl>,
    pub preds: HashMap<Ident, PredDecl>,
    pub querys: Vec<QueryDecl>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Rule<V = Ident> {
    pub vars: Vec<(V, TermType)>,
    pub head: Vec<TermVal<V>>,
    pub prims: Vec<(Prim, Vec<AtomVal<V>>)>,
    pub calls: Vec<(Ident, Vec<TermType>, Vec<TermVal<V>>)>,
}

impl Rule<Ident> {
    pub fn tag_ctx(&self, ctx: usize) -> Rule<IdentCtx> {
        let vars: Vec<(IdentCtx, TermType)> = self
            .vars
            .iter()
            .map(|(par, typ)| (par.tag_ctx(ctx), typ.clone()))
            .collect();

        let head: Vec<TermVal<IdentCtx>> = self.head.iter().map(|par| par.tag_ctx(ctx)).collect();

        let prims: Vec<(Prim, Vec<AtomVal<IdentCtx>>)> = self
            .prims
            .iter()
            .map(|(prim, args)| (*prim, args.iter().map(|arg| arg.tag_ctx(ctx)).collect()))
            .collect();

        let calls: Vec<(Ident, Vec<TermType>, Vec<TermVal<IdentCtx>>)> = self
            .calls
            .iter()
            .map(|(pred, poly, args)| {
                (
                    *pred,
                    poly.clone(),
                    args.iter().map(|arg| arg.tag_ctx(ctx)).collect(),
                )
            })
            .collect();

        Rule {
            vars,
            head,
            prims,
            calls,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct DataDecl {
    pub name: Ident,
    pub polys: Vec<Ident>,
    pub cons: Vec<Constructor>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Constructor {
    pub name: Ident,
    pub flds: Vec<TermType>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PredDecl {
    pub name: Ident,
    pub polys: Vec<Ident>,
    pub pars: Vec<(Ident, TermType)>,
    pub rules: Vec<Rule>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct QueryDecl {
    pub entry: Ident,
    pub params: Vec<QueryParam>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum QueryParam {
    DepthStep(usize),
    DepthLimit(usize),
    AnswerLimit(usize),
    AnswerPause(bool),
}

impl fmt::Display for PredDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let pars = self
            .pars
            .iter()
            .format_with(", ", |(var, typ), f| f(&format_args!("{}: {}", var, typ)));

        if self.polys.is_empty() {
            writeln!(f, "predicate {}({}):", self.name, pars)?;
        } else {
            let polys = self.polys.iter().format(", ");
            writeln!(f, "predicate {}[{}]({}):", self.name, polys, pars)?;
        }

        for rule in self.rules.iter() {
            write!(f, "{}", rule)?;
        }
        writeln!(f, "end")?;

        Ok(())
    }
}

impl<V: fmt::Display> fmt::Display for Rule<V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // let vars = self
        //     .vars
        //     .iter()
        //     .format_with(",", |(var, typ), f| f(&format_args!("{} : {}", var, typ)));

        if self.calls.is_empty() && self.prims.is_empty() {
            writeln!(f, "| ({}).", self.head.iter().format(", "))?;
            return Ok(());
        }

        writeln!(f, "| ({}) :-", self.head.iter().format(", "))?;

        for (prim, args) in self.prims.iter() {
            writeln!(f, "    {:?}({})", prim, args.iter().format(", "))?;
        }

        for (pred, polys, args) in self.calls.iter() {
            if polys.is_empty() {
                writeln!(f, "    {}({})", pred, args.iter().format(", "))?;
            } else {
                writeln!(
                    f,
                    "    {}[{}]({})",
                    pred,
                    polys.iter().format(", "),
                    args.iter().format(", ")
                )?;
            }
        }

        Ok(())
    }
}

impl fmt::Display for DataDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.polys.is_empty() {
            writeln!(
                f,
                "datatype {}[{}] where",
                self.name,
                self.polys.iter().format(", ")
            )?;
        } else {
            writeln!(f, "datatype {} where", self.name)?;
        }

        for cons in self.cons.iter() {
            write!(f, "{}", cons)?;
        }

        writeln!(f, "end")?;
        Ok(())
    }
}

impl fmt::Display for Constructor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "| {}({})", self.name, self.flds.iter().format(", "))
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (_data, data_decl) in self.datas.iter() {
            writeln!(f, "{}", data_decl)?;
        }

        for (_pred, pred_decl) in self.preds.iter() {
            writeln!(f, "{}", pred_decl)?;
        }

        Ok(())
    }
}

// `Goal` and `GoalPredDecl` are temporary data structure used inside the compiling pass
#[derive(Clone, Debug, PartialEq)]
pub(super) enum Goal {
    Lit(bool),
    Eq(TermVal, TermVal),
    Prim(Prim, Vec<AtomVal>),
    And(Vec<Goal>),
    Or(Vec<Goal>),
    Call(Ident, Vec<TermType>, Vec<TermVal>),
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct GoalPredDecl {
    pub name: Ident,
    pub polys: Vec<Ident>,
    pub pars: Vec<(Ident, TermType)>,
    pub vars: Vec<(Ident, TermType)>,
    pub goal: Goal,
}
