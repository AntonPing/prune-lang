use crate::utils::ident::Ident;
use crate::utils::lit::{LitType, LitVal};
use crate::utils::prim::Prim;

use itertools::Itertools;
use std::collections::HashMap;

use ast::*;
use term::*;

pub mod ast;
pub mod infer;
pub mod optimize;
pub mod term;
pub mod transform;
