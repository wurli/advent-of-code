use std::fs::read_to_string;

pub fn read_lines(filename: &str) -> Vec<String> {
    read_to_string(filename) 
        .unwrap()           // panic on possible file-reading errors
        .lines()            // split the string into an iterator of string slices
        .map(String::from)  // make each slice into a string
        .collect()          // gather them together into a vector
}

// Stolen from https://stackoverflow.com/questions/64498617/how-to-transpose-a-vector-of-vectors-in-rust
pub fn transpose<T>(v: Vec<Vec<T>>) -> Vec<Vec<T>> {
    assert!(!v.is_empty());
    let len = v[0].len();
    let mut iters: Vec<_> = v.into_iter().map(|n| n.into_iter()).collect();
    (0..len)
        .map(|_| {
            iters
                .iter_mut()
                .map(|n| n.next().unwrap())
                .collect::<Vec<T>>()
        })
        .collect()
}
