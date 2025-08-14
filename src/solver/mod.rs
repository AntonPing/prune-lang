use crate::logic::term::*;
use crate::utils::ident::*;
use crate::utils::lit::*;
use crate::utils::prim::*;

use self::atom::Atom;
use itertools::Itertools;
use std::fmt;

pub mod atom;
pub mod smt_z3;
pub mod solver;
pub mod subst;
