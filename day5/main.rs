use std::fs;
use std::collections::HashMap;

mod lines;

use lines::{Line, Coordinate};

const INPUT_PATH: &str = "day5/input.txt";

fn part1(lines: &Vec<Line>) {
    let mut counts: HashMap<Coordinate, u32> = HashMap::new();
    for point in lines.iter().flat_map(|line| line.points()) {
        *counts.entry(point).or_insert(0) += 1;
    }

    println!("Part 1: {}", counts.values().filter(|v| **v >= 2).count());
}

fn part2(lines: &Vec<Line>) {
    let mut counts: HashMap<Coordinate, u32> = HashMap::new();
    for point in lines.iter().flat_map(|line| line.points_diag()) {
        *counts.entry(point).or_insert(0) += 1;
    }

    println!("Part 2: {}", counts.values().filter(|v| **v >= 2).count());
}

fn main() {
    let raw = fs::read_to_string(INPUT_PATH)
        .expect("Error reading file");
    let lines = raw.trim().split("\n")
        .map(|raw_line| raw_line.parse::<Line>().expect("could not read line"))
        .collect::<Vec<Line>>();

    part1(&lines);
    part2(&lines);
}
