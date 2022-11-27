use std::fs;

const INPUT_PATH: &str = "day10/input.txt";

fn main() {
    let input = fs::read_to_string(INPUT_PATH).expect("error reading file");

    let (sanitized, part1score) = part1(&input);
    println!("Part 1: {}", part1score);
    println!("Part 2: {}", part2(&sanitized))
}

// convert a closed char to their open counterpart
fn closed_to_open(input: char) -> char {
    match input {
        ')' => '(',
        ']' => '[',
        '}' => '{',
        '>' => '<',
        _ => panic!("{input} is not a valid token")
    }
}

/// convert an open char to the appropriate syntax score
fn open_to_syntax_score(input: char) -> u64 {
    match input {
        '(' => 3,
        '[' => 57,
        '{' => 1197,
        '<' => 25137,
        _ => panic!("{input} is not a valid token")
    }
}

/// counts the syntax errors from mismatched tags, returns a sanitized list of valid lines
fn part1(input: &str) -> (Vec<Vec<char>>, u64) {
    let mut score = 0;

    let stacks = input.split("\n").filter_map(|line| {
        let mut stack = Vec::new();
        for token in line.trim().chars() {
            match token {
                // opening chars just get added to the stack
                '(' | '[' | '{' | '<' => {
                    stack.push(token);
                },
                // closing chars are checked against the top of the stack
                ')' | ']' | '}' | '>' => {
                    // get the open representation of the char
                    let open_c = closed_to_open(token);
                    match stack.pop() {
                        Some(x) => {
                            if x != open_c {
                                // mismatched closing char
                                score += open_to_syntax_score(open_c);
                                return None;
                            }
                        },
                        // stack is empty, unexpected closing char
                        None => {
                            score += open_to_syntax_score(open_c);
                            return None;
                        },
                    }
                },
                _ => panic!("{token} is not a valid char"),
            }
        }
        return Some(stack);
    }).collect::<Vec<Vec<char>>>();

    return (stacks, score);
}

/// convert an open char to the appropriate autocomplete score
fn open_to_autocomplete_score(input: &char) -> u64 {
    match input {
        '(' => 1,
        '[' => 2,
        '{' => 3,
        '<' => 4,
        _ => panic!("{input} is not a valid token")
    }
}

/// convert a closed char to their open counterpart
fn open_to_closed(input: char) -> char {
    match input {
        '(' => ')',
        '[' => ']',
        '{' => '}',
        '<' => '>',
        _ => panic!("{input} is not a valid token")
    }
}

/// given a stack from part2, create the appropriate completion
fn autocomplete(line: &Vec<char>) -> String {
    let mut completion= "".to_owned();
    for token in line.iter().rev() {
        completion.push_str(&open_to_closed(*token).to_string())
    }
    return completion
}

fn completion_score(completion: &str) -> u64 {
    let mut score = 0;
    for token in completion.chars() {
        score *= 5;
        score += open_to_autocomplete_score(&closed_to_open(token));
    }
    return score;
}

fn part2(input: &Vec<Vec<char>>) -> u64 {
    let mut scores = input.iter().map(|line| completion_score(&autocomplete(line))).collect::<Vec<u64>>();
    scores.sort();
    // println!("{}", scores.len() / 2 + 1);
    // println!("{:?}", scores);
    return scores[scores.len() / 2 + 1]
}

#[cfg(test)]
mod tests {
    // Import names from outer "lines" scope (via https://doc.rust-lang.org/rust-by-example/testing/unit_testing.html)
    use super::*;

    const INPUT: &str = "[({(<(())[]>[[{[]{<()<>>
        [(()[<>])]({[<{<<[]>>(
        {([(<{}[<>[]}>{[]{[(<()>
        (((({<>}<{<{<>}{[]{[]{}
        [[<[([]))<([[{}[[()]]]
        [{[{({}]{}}([{[{{{}}([]
        {<[[]]>}<{[{[{[]{()[[[]
        [<(<(<(<{}))><([]([]()
        <{([([[(<>()){}]>(<<{{
        <{([{{}}[<[[[<>{}]]]>[]]";

    #[test]
    fn test_part1() {
        let (sanitized, part1score) = part1(&INPUT);
        assert_eq!(sanitized.len(), 5);
        assert_eq!(part1score, 26397);
    }

    #[test]
    fn test_completion_score() {
        assert_eq!(completion_score("}}]])})]"), 288957);
        assert_eq!(completion_score(")}>]})"), 5566);
        assert_eq!(completion_score("}}>}>))))"), 1480781);
        assert_eq!(completion_score("]]}}]}]}>"), 995444);
        assert_eq!(completion_score("])}>"), 294);
    }

    // #[test]
    // fn test_part2() {
    //     let (sanitized, _) = part1(&INPUT);
    //     assert_eq!(part2(&sanitized), 288957)
    // }
}
