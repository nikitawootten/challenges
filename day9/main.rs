use std::fs;

use std::collections::VecDeque;

const INPUT_PATH: &str = "day9/input.txt";

fn main() {
    let raw = fs::read_to_string(INPUT_PATH).expect("error reading file");
    let input = parse_input(&raw);

    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}

fn parse_input(input: &str) -> Vec<Vec<u8>> {
    input
        .trim()
        .split("\n")
        .map(|line| {
            line.chars()
                .map(|point| {
                    point
                        .to_string()
                        .parse::<u8>()
                        .expect("Could not parse point")
                })
                .collect()
        })
        .collect()
}

fn part1(input: &Vec<Vec<u8>>) -> u32 {
    let mut risk_level = 0;
    for (i, line) in input.iter().enumerate() {
        for (j, point) in line.iter().enumerate() {
            if (i != 0 && input[i - 1][j] <= *point)
                || (i + 1 < input.len() && input[i + 1][j] <= *point)
                || (j != 0 && line[j - 1] <= *point)
                || (j + 1 < line.len() && line[j + 1] <= *point)
            {
                continue;
            }
            risk_level += 1 + *point as u32;
        }
    }
    return risk_level;
}

fn part2(input: &Vec<Vec<u8>>) -> u32 {
    let mut basins: Vec<Vec<(usize, usize)>> = Vec::new();
    for i in 0..input.len() {
        for j in 0..input[i].len() {
            let mut basin: Vec<(usize, usize)> = Vec::new();
            let mut candidates = VecDeque::from([(i, j)]);

            'cand_check: while !candidates.is_empty() {
                let (c_i, c_j) = candidates.pop_front().expect("expected to be not empty");

                if input[c_i][c_j] < 9 {
                    // check the current basin for this candidate
                    for (e_i, e_j) in basin.iter() {
                        if *e_i == c_i && *e_j == c_j {
                            continue 'cand_check;
                        }
                    }

                    // check past basins for this candidate
                    for b in basins.iter() {
                        for (e_i, e_j) in b {
                            if *e_i == c_i && *e_j == c_j {
                                continue 'cand_check;
                            }
                        }
                    }

                    basin.push((c_i, c_j));

                    if c_i != 0 {
                        candidates.push_back((c_i - 1, c_j));
                    }

                    if c_i < input.len() - 1 {
                        candidates.push_back((c_i + 1, c_j));
                    }

                    if c_j != 0 {
                        candidates.push_back((c_i, c_j - 1));
                    }

                    if c_j < input[c_i].len() - 1 {
                        candidates.push_back((c_i, c_j + 1));
                    }
                }
            }

            if basin.len() > 0 {
                basins.push(basin);
            }
        }
    }

    basins.sort_by_key(|basin| basin.len());
    basins.reverse();

    let mut score = 1;
    for i in 0..3 {
        score *= basins[i].len() as u32;
    }
    return score;
}

#[cfg(test)]
mod tests {
    // Import names from outer "lines" scope (via https://doc.rust-lang.org/rust-by-example/testing/unit_testing.html)
    use super::*;

    const INPUT: &str = "2199943210
3987894921
9856789892
8767896789
9899965678";

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse_input(&INPUT)), 15)
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse_input(&INPUT)), 1134)
    }
}
