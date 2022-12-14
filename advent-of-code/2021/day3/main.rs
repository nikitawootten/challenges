use std::fs;

const INPUT_PATH: &str = "day3/input.txt";

fn part1(input: &str) -> Vec<u32> {
    let lines = input.trim().split("\n").collect::<Vec<&str>>();
    let lines_len = lines.len() as u32;
    let mut bit_counts: Vec<u32> = vec![0; lines[0].len()];
    for line in lines {
        for (i, char) in line.chars().enumerate() {
            bit_counts[i] += char.to_digit(2)
                .expect(format!("char '{}' should be a binary digit", char.to_string()).as_str())
        }
    }

    let mut gamma_rate = 0 as u32;
    let mut epsilon_rate = 0 as u32;

    for count in bit_counts.clone() {
        let most_common = if count >= (lines_len / 2) { 1 } else { 0 };
        gamma_rate = (gamma_rate << 1) + most_common;
        epsilon_rate = (epsilon_rate << 1) + (1 - most_common);
    }

    println!("Part 1: {}", gamma_rate * epsilon_rate);
    return bit_counts;
}

fn filter_candidates(input: &Vec<&str>, criteria: impl Fn(usize, u32) -> bool) -> u32 {
    let mut candidates = input.to_vec();

    for i in 0..candidates[0].len() {
        candidates = candidates.into_iter().filter(|candidate| {
            let char = candidate.chars().nth(i)
                .expect(format!("could got get {i}th item of the candidate").as_str());
            let val = char.to_digit(2)
                .expect(format!("char '{}' should be a binary digit", char.to_string()).as_str());
            criteria(i, val)
        }).collect();

        if candidates.len() == 1 {
            break;
        }
    }

    if candidates.len() != 1 {
        panic!("invalid number of candidates: {}", candidates.len())
    }

    let mut result = 0 as u32;
    
    for char in candidates[0].chars().rev() {
        let val = char.to_digit(2)
            .expect(format!("char '{}' should be a binary digit", char.to_string()).as_str());
        result = (result << 1) + val;
    }

    return result;
}

fn part2(input: &str, bit_counts: Vec<u32>) {
    let lines = input.trim().split("\n").collect::<Vec<&str>>();
    let lines_len = lines.len();

    let oxygen_rating = filter_candidates(&lines, |i, val| {
        (if bit_counts[i] >= (lines_len / 2) as u32 { 1 } else { 0 }) == val
    });

    let carbon_dioxide_rating = filter_candidates(&lines, |i, val| {
        (if bit_counts[i] <= (lines_len / 2) as u32 { 0 } else { 1 }) == val
    });

    println!("Part 2: {}", oxygen_rating * carbon_dioxide_rating);
}

fn main() {
    let raw = fs::read_to_string(INPUT_PATH)
        .expect("Error reading file");
    let bit_counts = part1(&raw);
    part2(&raw, bit_counts);
}
