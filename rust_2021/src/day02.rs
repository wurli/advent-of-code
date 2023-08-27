use crate::utils;

struct Sub1 {
    pos_v: i32,
    pos_h: i32
}

impl Sub1 {
    fn      up(&mut self, amount: i32) { self.pos_v -= amount; }
    fn    down(&mut self, amount: i32) { self.pos_v += amount; }
    fn forward(&mut self, amount: i32) { self.pos_h += amount; }
}

struct Sub2 {
    pos_v: i32,
    pos_h: i32,
    aim: i32
}

impl Sub2 {
    fn      up(&mut self, amount: i32) { self.aim -= amount; }
    fn    down(&mut self, amount: i32) { self.aim += amount; }
    fn forward(&mut self, amount: i32) {
        self.pos_h += amount;
        self.pos_v += amount * self.aim;
    }
}

pub fn solve() -> String {
    let input = utils::read_lines("inputs/day02.txt");

    let res: Vec<(_, _)> = input.
        iter().
        map(|s| {
            let mut parts = s.split_whitespace();
            (parts.next().unwrap(), parts.next().unwrap())
        }).
        collect();

    let mut sub1 = Sub1 {pos_h: 0, pos_v: 0};
    let mut sub2 = Sub2 {pos_h: 0, pos_v: 0, aim: 0};

    for e in res.iter() {
        let dir = e.0;
        let amount = e.1.parse::<i32>().unwrap();

        match dir {
            "up"      => {
                sub1.up(amount);
                sub2.up(amount);
            },
            "down"    => {
                sub1.down(amount);
                sub2.down(amount);
            },
            "forward" => {
                sub1.forward(amount);
                sub2.forward(amount)
            },
            &_ => todo!()
        }
    }

    format!(
        "Part 1: {}\nPart 2: {}", 
        sub1.pos_h * sub1.pos_v,
        sub2.pos_h * sub2.pos_v
    )
}