use crate::utils::ident::Ident;
use crate::utils::lit::LitVal;
use crate::utils::prim::Prim;
use term::*;

use itertools::Itertools;
use std::collections::HashMap;
use std::fmt::{Debug, Display};

pub mod logic;
pub mod solver;
pub mod term;
pub mod unify;
