pub mod day01;
pub mod day02;
pub mod utils;

fn main() {
    println!(
        "-- Day 01 --------\n{}", 
        day01::solve()
    );
    println!(
        "-- Day 02 --------\n{}", 
        day02::solve()
    );
}
