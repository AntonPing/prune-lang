use crate::utils::ident::Ident;
use crate::utils::lit::*;
use crate::utils::prim::Prim;
use crate::utils::term::*;

use itertools::Itertools;
use std::collections::HashMap;

use ast::*;

pub mod ast;
pub mod compile;
pub mod elab;
