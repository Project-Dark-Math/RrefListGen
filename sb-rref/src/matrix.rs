use std::fmt::{self, Display};
use std::ops::{Index, IndexMut};

use crate::field::Field;
use crate::row_operation::ElemRowOp;

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub struct Matrix<T> {
    pub row: usize,
    pub col: usize,
    pub inner: Vec<T>,
}

impl<T> Matrix<T> {
    pub fn new(row: usize, col: usize, inner: Vec<T>) -> Self {
        assert_eq!(row * col, inner.len());
        Self { row, col, inner }
    }
}

impl<T: Clone> Matrix<T> {
    pub fn clone_row(&self, row: usize) -> Vec<T> {
        self.inner.chunks(self.col).nth(row - 1).unwrap().to_vec()
    }
}

impl<T: Field> Matrix<T> {
    pub fn do_elem_op(&mut self, elem_op: ElemRowOp<T>) {
        match elem_op {
            ElemRowOp::RowExchange(row1, row2) => {
                let tmp_row = self.clone_row(row1);

                for i in 1..=self.col {
                    self[(row1, i)] = self[(row2, i)];
                    self[(row2, i)] = tmp_row[i - 1];
                }
            }
            ElemRowOp::RowScalarMultiple { row, by } => {
                for i in 1..=self.col {
                    self[(row, i)] = self[(row, i)].mul(by);
                }
            }
            ElemRowOp::RowAddition { to, from, by } => {
                for i in 1..=self.col {
                    self[(to, i)] = self[(to, i)].add(self[(from, i)].mul(by));
                }
            }
        }
    }
}

impl<T> Index<(usize, usize)> for Matrix<T> {
    type Output = T;
    fn index(&self, idx: (usize, usize)) -> &Self::Output {
        &self.inner[(idx.0 - 1) * self.col + (idx.1 - 1)]
    }
}

impl<T> IndexMut<(usize, usize)> for Matrix<T> {
    fn index_mut(&mut self, idx: (usize, usize)) -> &mut Self::Output {
        &mut self.inner[(idx.0 - 1) * self.col + (idx.1 - 1)]
    }
}

impl<T: Display> Display for Matrix<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut buffer = String::new();

        for r in 1..=self.row {
            for c in 1..=self.col {
                buffer += &format!("{} ", self[(r, c)]);
            }
            buffer.pop();
            buffer += "\n";
        }

        write!(f, "{}", buffer)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::field::f5::F5::{Four, One, Three, Two, Zero};

    #[test]
    fn row_exchange() {
        let mut matrix = Matrix::new(
            3,
            3,
            vec![One, Two, Three, Four, One, Two, Three, Zero, Four],
        );
        let expected = vec![One, Two, Three, Three, Zero, Four, Four, One, Two];

        matrix.do_elem_op(ElemRowOp::RowExchange(2, 3));

        assert_eq!(matrix.inner, expected);
    }

    #[test]
    fn row_scalar_multiple() {
        let mut matrix = Matrix::new(
            3,
            3,
            vec![One, Two, Three, Four, One, Two, Three, Zero, Four],
        );
        let expected = vec![One, Two, Three, Three, Two, Four, Three, Zero, Four];

        matrix.do_elem_op(ElemRowOp::RowScalarMultiple { row: 2, by: Two });

        assert_eq!(matrix.inner, expected);
    }

    #[test]
    fn row_addition() {
        let mut matrix = Matrix::new(
            3,
            3,
            vec![One, Two, Three, Four, One, Two, Three, Zero, Four],
        );
        let expected = vec![Zero, Two, Zero, Four, One, Two, Three, Zero, Four];

        matrix.do_elem_op(ElemRowOp::RowAddition {
            to: 1,
            from: 3,
            by: Three,
        });

        assert_eq!(matrix.inner, expected);
    }
}
