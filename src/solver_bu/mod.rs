use crate::logic::term::*;
use crate::utils::ident::Ident;
// use crate::utils::lit::LitVal;
use crate::utils::prim::Prim;

use itertools::Itertools;
use std::fmt::{Debug, Display};

pub mod constr;
pub mod smt_z3;
pub mod solution;
pub mod solver;
pub mod unify;
