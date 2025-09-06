// use crate::syntax::lexer::Span;
use crate::utils::ident::Ident;
use crate::utils::lit::LitType;
use crate::utils::term::*;

use std::collections::HashMap;

pub mod check;
pub mod elab;
pub mod rename;
pub mod unify;
