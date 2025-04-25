use crate::logic::term::*;
use crate::logic::trans::PredIdent;
use crate::utils::ident::Ident;
use crate::utils::lit::LitVal;
use crate::utils::prim::{Compare, Prim};

use itertools::Itertools;
use term::{IdentCtx, TermCtx};

pub mod codes;
pub mod compile;
pub mod smt_z3;
pub mod solver;
pub mod subst;
pub mod term;
