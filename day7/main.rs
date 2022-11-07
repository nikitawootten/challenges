use std::fs;

const INPUT_PATH: &str = "day7/input.txt";

fn part1(positions: &Vec<u32>) -> (u32, u32) {
    let median = {
        let mut sorted = positions.clone();
        sorted.sort();
        sorted[sorted.len() / 2]
    };

    let mut cost = 0;
    for i in positions {
        cost += median.abs_diff(*i);
    }

    return (median, cost);
}

fn cost2(positions: &Vec<u32>, position: u32) -> u32 {
    let mut cost = 0;
    for i in positions {
        let diff = position.abs_diff(*i);
        cost += diff * (diff + 1) / 2;
    }
    return cost;
}

fn part2(positions: &Vec<u32>) -> (u32, u32) {
    let mean = {
        let mut sum = 0;
        for i in positions {
            sum += *i;
        }
        ((sum as f32) / (positions.len() as f32)) as u32
    };

    // there seems to be some abiguity to minimum costs that isn't solved with rounding
    let cost_floor = cost2(&positions, mean);
    let cost_ceil = cost2(&positions, mean + 1);

    if cost_floor < cost_ceil {
        return (mean, cost_floor);
    } else {
        return (mean + 1, cost_ceil);
    }
}

fn main() {
    let raw = fs::read_to_string(INPUT_PATH).expect("error reading file");
    let input = raw
        .trim()
        .split(",")
        .map(|token| token.parse::<u32>().expect("could not parse token"))
        .collect::<Vec<u32>>();

    println!("Part 1: {}", part1(&input).1);
    println!("Part 2: {}", part2(&input).1);
}

#[cfg(test)]
mod tests {
    // Import names from outer "lines" scope (via https://doc.rust-lang.org/rust-by-example/testing/unit_testing.html)
    use super::*;

    const TEST_INPUT: [u32; 10] = [16, 1, 2, 0, 4, 2, 7, 1, 2, 14];

    #[test]
    fn test_part1() {
        let (median, cost) = part1(&Vec::from(TEST_INPUT));
        assert_eq!(median, 2);
        assert_eq!(cost, 37);
    }

    #[test]
    fn test_part2() {
        let (mean, cost) = part2(&Vec::from(TEST_INPUT));
        assert_eq!(mean, 5);
        assert_eq!(cost, 168);
    }
}
