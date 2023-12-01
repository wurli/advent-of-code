use crate::matrix::matrix;

pub mod day01;
pub mod day02;
pub mod day03;
pub mod utils;
pub mod matrix;

fn main() {
    println!(
        "-- Day 01 --------\n{}", 
        day01::solve()
    );
    println!(
        "-- Day 02 --------\n{}", 
        day02::solve()
    );
    println!(
        "-- Day 03 --------\n{}", 
        day03::solve()
    );

    let content = vec![
        1, 2, 3, 
        4, 5, 6, 
        7, 8, 9
    ];
    let m = matrix(content, 3);

    println!("Col 2: {:#?}", m.transpose().col(2));

}
