use crate::utils::ident::*;
use crate::utils::lit::*;
use crate::utils::prim::*;
use crate::utils::term::*;

use itertools::Itertools;
use std::collections::HashMap;

use ast::*;

pub mod ast;
pub mod optimize;
pub mod transform;
