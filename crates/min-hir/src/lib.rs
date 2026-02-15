pub mod hir;
pub mod lower;
pub mod check;

pub use hir::*;
pub use lower::LoweringContext;
pub use check::{type_check, TypeCheckResult};
