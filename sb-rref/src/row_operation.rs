use std::fmt::{self, Display};

#[derive(Debug, Clone, Copy)]
pub enum ElemRowOp<T> {
    RowExchange(usize, usize),
    RowScalarMultiple { row: usize, by: T },
    RowAddition { to: usize, from: usize, by: T },
}

impl<T: Display> Display for ElemRowOp<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::RowExchange(row1, row2) => write!(f, "RowExchange {row1} {row2}"),
            Self::RowScalarMultiple { row, by } => write!(f, "RowScalarMultiple {row} {by}"),
            Self::RowAddition { to, from, by } => write!(f, "RowAddition {to} {from} {by}"),
        }
    }
}
