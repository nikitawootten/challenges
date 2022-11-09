use std::fs;

const INPUT_PATH: &str = "day8/input.txt";

fn part1(input: &str) -> u32 {
    // counter for the number of "1, 4, 7, or 8 segments"
    let mut simple_nums = 0;
    input.trim().split("\n").for_each(|line| {
        let sections = line.split("|").collect::<Vec<&str>>();
        if sections.len() != 2 {
            panic!("line has incorrect number of sections");
        }

        let out_section = sections[1];

        out_section
            .split(" ")
            .for_each(|stanza| match stanza.len() {
                // matches 1, 7, 4, and 8 respectively
                2 | 3 | 4 | 7 => simple_nums += 1,
                _ => {}
            });
    });
    return simple_nums;
}

fn part2(input: &str) -> u32 {
    input
        .trim()
        .split("\n")
        .map(|line| {
            let sections = line.split("|").collect::<Vec<&str>>();
            if sections.len() != 2 {
                panic!("line has incorrect number of sections");
            }

            // contains all 10 digits that could be displayed
            // each stanza's internal order of digits is random
            let in_section = sections[0];
            // contains the 4 output digits we actually care about
            let out_section = sections[1];

            let mut signal_counts = [0 as u8; 7];

            // will contain the letters that make up the known value for 7
            // this can be used to differentiate 0 from 6
            // (6 contains a segment not shared between 0 or 7)
            let mut seven_signals = "";

            for stanza in in_section.trim().split(" ") {
                for letter in stanza.chars() {
                    signal_counts[letter as usize - 97] += 1
                }

                if stanza.len() == 3 {
                    // known to be the 7
                    seven_signals = stanza
                }
            }

            // known segment values
            //  a  |
            // b c | b, e, and f are easy to deduce from the signal counts
            //  d  | (b appears a total of 6 times in numbers 0-9, etc.)
            // e f | b and e specifically can be used to identify 2, 3, 5, or 9
            //  g  |
            let b_segment = (signal_counts
                .iter()
                .position(|&x| x == 6)
                .expect("invalid input section, no b segment")
                + 97) as u8 as char;
            let e_segment = (signal_counts
                .iter()
                .position(|&x| x == 4)
                .expect("invalid input section, no e segment")
                + 97) as u8 as char;

            return out_section
                .trim()
                .split(" ")
                .map(move |stanza| {
                    return match stanza.len() {
                        2 => 1,
                        3 => 7,
                        4 => 4,
                        5 => {
                            // could be either 2, 3, or 5
                            if stanza.contains(b_segment) {
                                return 5;
                            } else if stanza.contains(e_segment) {
                                return 2;
                            } else {
                                return 3;
                            }
                        }
                        6 => {
                            // could be either 0, 6, or 9
                            if stanza.contains(e_segment) {
                                for letter in seven_signals.chars() {
                                    if !stanza.contains(letter) {
                                        return 6;
                                    }
                                }
                                return 0;
                            } else {
                                return 9;
                            }
                        }
                        7 => 8,
                        _ => panic!("invalid number of segments in input stanza"),
                    };
                })
                // convert mapped individual digits to single accumulated value
                .fold(0, |acc, i| acc * 10 + i);
        })
        .sum()
}

fn main() {
    let input = fs::read_to_string(INPUT_PATH).expect("error reading file");
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}

#[cfg(test)]
mod tests {
    // Import names from outer "lines" scope (via https://doc.rust-lang.org/rust-by-example/testing/unit_testing.html)
    use super::*;

    const INPUT: &str =
        "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce";

    #[test]
    fn test_part1() {
        let result = part1(INPUT);
        assert_eq!(result, 26);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(INPUT), 61229);
    }
}
