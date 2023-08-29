use crate::utils;

fn most_common_bit(x: Vec<u32>) -> u32 {
    let num_ones: u32 = x.iter().sum();
    let num_zeros = x.len() as u32 - num_ones;
    (num_ones >= num_zeros) as u32
}

fn least_common_bit(x: Vec<u32>) -> u32 {
    (most_common_bit(x) == 0) as u32
}

fn invert(x: Vec<u32>) -> Vec<u32> {
    x
        .iter()
        .map(|&xi| (xi != 1) as u32)
        .collect()
}

fn bin2dec(x: &Vec<u32>) -> u32 {
    let combined1 = x
        .iter()
        .map(|xi| xi.to_string())
        .collect::<Vec<String>>()
        .join("");
        
    let combined = combined1.as_str();

    u32::from_str_radix(combined, 2)
        .expect("Couldn't parse binary string")
}

fn solve_p2(x: Vec<Vec<u32>>, test: &dyn Fn(Vec<u32>) -> u32) -> u32 {
    let bit_positions = 0..x[0].len();

    let last_num = &bit_positions
        .fold(x, |x, pos| {
            if x.len() == 1 {
                return x
            }
            let col = x.iter().map(|xi| xi[pos]).collect::<Vec<u32>>();
            let test_val = test(col);
            x
                .into_iter()
                .filter(|xi| xi[pos] == test_val)
                .collect()
        });
    
    bin2dec(&last_num[0])
}

pub fn solve() -> String {
    let input = utils::read_lines("inputs/day03.txt");

    let nums: Vec<Vec<u32>> = input
        .iter()
        .map(|line| line
            .chars()
            .map(|c| c.to_digit(10).unwrap())
            .collect()
        )
        .collect();

    let nums_t = utils::transpose(nums.clone());

    let most_common_bits: Vec<u32> = nums_t
        .iter()
        .map(|x| most_common_bit(x.to_vec()))
        .collect();

    let p1 = bin2dec(&most_common_bits) * bin2dec(&invert(most_common_bits));
    let p2 = solve_p2(nums.clone(), &most_common_bit) * solve_p2(nums.clone(), &least_common_bit);

    format!("Part 1: {}\nPart 2: {}", p1, p2)
}