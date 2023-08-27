use crate::utils;

fn n_increases(nums: &Vec<i32>, window: i32) -> i32 {

    let n: i32 = nums.len().try_into().unwrap();
    let indices: Vec<i32> = (0..(n - window + 1)).step_by(1).collect();

    let vals: Vec<i32> = indices
        .iter()
        .map(|i| {
            let rng = i.to_owned() as usize..(i + window) as usize;
            nums[rng].iter().sum()
        })
        .collect();

    let first_items = &vals[0..(vals.len() - 1)];
    let next_items  = &vals[1..(vals.len())];

    let first_and_next_items = first_items.into_iter().zip(next_items);

    let res: i32 = first_and_next_items.
        map(|x| (x.1 > x.0) as i32)
        .collect::<Vec<i32>>()
        .iter()
        .sum();

    res

}

pub fn solve() -> String {
    let input = utils::read_lines("inputs/day01.txt");

    let input_parsed: Vec<i32> = input.
        iter().
        map(|s| s.parse::<i32>().unwrap()).
        collect();        

    format!(
        "Part 1: {}\nPart 2: {}",
        n_increases(&input_parsed, 1),
        n_increases(&input_parsed, 3)
    )
}
