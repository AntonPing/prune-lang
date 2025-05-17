use crate::logic::term::*;
use crate::utils::ident::{Ident, IdentCtx};
use crate::utils::lit::{LitType, LitVal};
use crate::utils::prim::{Compare, Prim};

use itertools::Itertools;

use std::fmt;

pub mod smt_z3;
pub mod solver;
pub mod subst;
