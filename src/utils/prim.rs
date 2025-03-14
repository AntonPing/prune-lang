#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub enum Compare {
    Lt,
    Le,
    Eq,
    Ge,
    Gt,
    Ne,
}

#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub enum Prim {
    /// integer arithmetics
    IAdd,
    ISub,
    IMul,
    IDiv,
    IRem,
    INeg,

    /// float-point arithmetics
    // FAdd,
    // FSub,
    // FMul,
    // FDiv,
    // FNeg,

    /// comparision
    ICmp(Compare),

    /// boolean operation
    BAnd,
    BOr,
    BNot,
}
