use crate::utils::ident::Ident;
use crate::utils::lit::LitVal;
use crate::utils::prim::Prim;
use term::*;

use itertools::Itertools;
use std::collections::HashMap;
use std::fmt::{Debug, Display};

pub mod constr;
pub mod logic;
pub mod smt;
pub mod solution;
pub mod solver;
pub mod term;
pub mod unify;
