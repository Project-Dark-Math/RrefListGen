use std::fmt::{Debug, Display};

use crate::field::Field;
use crate::matrix::Matrix;
use crate::pretty_print::pretty_print;
use crate::row_operation::ElemRowOp;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OracleState {
    Running,
    Almost,
    Finished,
}

#[derive(Debug)]
pub struct Oracle<T> {
    pub matrix: Matrix<T>,
    pivot_loc: (usize, usize),
    location: (usize, usize),
    pub record_string: String,
    state: OracleState,
}

impl<T> Oracle<T> {
    pub fn new(matrix: Matrix<T>) -> Self {
        Self {
            matrix,
            pivot_loc: (1, 1),
            location: (1, 1),
            record_string: String::new(),
            state: OracleState::Running,
        }
    }
}

impl<T> Oracle<T>
where
    T: Debug + Display + Clone + Field,
{
    pub fn run(&mut self) {
        while self.state == OracleState::Running {
            self.find_col_pivot();
            self.change_if_necessary();
            self.make_pivot_one_col();
        }

        self.move_zero_to_bottom();
    }

    pub fn find_col_pivot(&mut self) {
        while self.state != OracleState::Almost {
            if self.location.1 > self.matrix.col {
                self.state = OracleState::Almost;
                break;
            }

            if self.location.0 > self.matrix.row {
                self.location = (self.pivot_loc.0, self.location.1 + 1);
                continue;
            }

            if self.matrix[self.location] != T::add_ident() {
                self.pivot_loc = (self.location.0, self.location.1);
                self.location = (1, self.location.1);
                break;
            }

            self.location = (self.location.0 + 1, self.location.1);
        }
    }

    pub fn change_if_necessary(&mut self) {
        if self.state == OracleState::Almost {
            return;
        }

        if self.pivot_loc.0 > self.pivot_loc.1 {
            let elem = ElemRowOp::RowExchange(self.pivot_loc.0, self.pivot_loc.1);

            self.matrix.do_elem_op(elem);
            self.pivot_loc = (self.pivot_loc.1, self.pivot_loc.1);
            self.location = (1, self.pivot_loc.1);

            self.record_string += &format!("{elem}\n");
        }
    }

    pub fn make_pivot_one_col(&mut self) {
        if self.location.1 > self.matrix.col {
            self.state = OracleState::Almost;
            return;
        }

        if self.location.0 > self.matrix.row {
            return;
        }

        if self.location.1 != self.pivot_loc.1 {
            panic!("Internal Error");
        }

        let mut n = 1;
        loop {
            if n > self.matrix.row {
                self.next_col();
                break;
            }

            if n == self.pivot_loc.0 {
                if self.matrix[self.pivot_loc] != T::mul_ident() {
                    let elem = ElemRowOp::RowScalarMultiple {
                        row: n,
                        by: self.matrix[self.pivot_loc].mul_inv(),
                    };

                    self.matrix.do_elem_op(elem);
                    self.record_string += &format!("{elem}\n");
                    self.record_string +=
                        &format!("{}\n\n", pretty_print(self.matrix.clone_row(n)));
                }
            } else {
                let pivot = self.matrix[self.pivot_loc].mul_inv();
                let val = self.matrix[(n, self.location.1)].add_inv().mul(pivot);

                if val != T::add_ident() {
                    let elem = ElemRowOp::RowAddition {
                        to: n,
                        from: self.pivot_loc.0,
                        by: val,
                    };

                    self.matrix.do_elem_op(elem);
                    self.record_string += &format!("{elem}\n");
                    self.record_string +=
                        &format!("{}\n\n", pretty_print(self.matrix.clone_row(n)));
                }
            }

            self.location.0 = n + 1;
            n += 1;
        }
    }

    pub fn move_zero_to_bottom(&mut self) {
        let mut k = 1;

        loop {
            if k >= self.matrix.row {
                self.state = OracleState::Finished;
                break;
            }

            if self
                .matrix
                .clone_row(k)
                .into_iter()
                .all(|val| val == T::add_ident())
            {
                let elem = ElemRowOp::RowExchange(k, k + 1);

                self.matrix.do_elem_op(elem);
                self.record_string += &format!("{elem}\n");
            }

            k += 1;
        }
    }

    fn next_col(&mut self) {
        if self.matrix.col < self.pivot_loc.1 {
            panic!("Index is out of bound");
        }

        self.pivot_loc = (self.pivot_loc.0 + 1, self.pivot_loc.1 + 1);
        self.location = self.pivot_loc;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::field::f5::F5::{Four, One, Three, Two, Zero};

    #[test]
    fn row_exchange() {
        let matrix = Matrix::new(
            3,
            3,
            vec![Four, One, Two, Three, Zero, Four, One, Two, Three],
        );
        let expected = Matrix::new(
            3,
            3,
            vec![One, Zero, Three, Zero, One, Zero, Zero, Zero, Zero],
        );

        let mut oracle = Oracle::new(matrix);
        oracle.run();

        println!("{:#?}", oracle);

        assert_eq!(oracle.matrix, expected);
    }
}
