use crate::utils::ident::Ident;
use crate::utils::lit::{LitType, LitVal};
use crate::utils::prim::Prim;
use itertools::Itertools;
use term::*;

use std::collections::HashMap;

pub mod infer;
pub mod term;
pub mod trans;
