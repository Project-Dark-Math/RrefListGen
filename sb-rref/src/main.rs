mod field;
mod matrix;
mod oracle;
mod pretty_print;
mod row_operation;

use std::env;
use std::fs::File;
use std::io::{self, prelude::*};
use std::str::FromStr;

use num_rational::Rational64;

use pretty_print::pretty_print;

fn main() -> io::Result<()> {
    let mut args = env::args();
    args.next();

    let filename = args.next().unwrap_or("./input.txt".to_string());

    let mut file = File::open(&filename)?;
    let mut buffer = String::new();
    file.read_to_string(&mut buffer)?;

    let mut split = buffer.split(char::is_whitespace);
    let row = split.next().unwrap().parse::<usize>().unwrap();
    let col = split.next().unwrap().parse::<usize>().unwrap();
    let nums = split
        .filter(|val| !val.is_empty())
        .map(|val| Rational64::from_str(val).unwrap())
        .collect::<Vec<Rational64>>();

    let matrix = matrix::Matrix::new(row, col, nums);
    let mut oracle = oracle::Oracle::new(matrix);

    oracle.run();

    println!("{}", buffer);
    println!("{}", oracle.record_string);
    println!("{}", oracle.matrix);

    Ok(())
}
