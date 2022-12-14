use std::fs;

mod lanternfish;

use lanternfish::School;

const INPUT_PATH: &str = "day6/input.txt";

fn part1(mut school: School) {
    let size = school.simulate_days(80);
    println!("Part 1: {}", size);
}

fn part2(mut school: School) {
    let size = school.simulate_days(256);
    println!("Part 2: {}", size);
}

fn main() {
    let input = fs::read_to_string(INPUT_PATH)
        .expect("error reading file");
    let school = input.parse::<School>()
        .expect("could not parse school of fish");
    part1(school.clone());
    part2(school);
}
