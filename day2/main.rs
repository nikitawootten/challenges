use std::{fs};
use std::str::FromStr;

const INPUT_PATH: &str = "day2/input.txt";

#[derive(PartialEq)]
enum Direction {
    Forward,
    Down,
    Up
}

impl FromStr for Direction {
    type Err = String;
    fn from_str(input: &str) -> Result<Direction, Self::Err> {
        match input { 
            "forward" => Ok(Direction::Forward),
            "up" => Ok(Direction::Up),
            "down" => Ok(Direction::Down),
            _ => Err(format!("Could not parse direction '{}', expected forward, up, or down", input))
        }
    }
}

struct Command {
    direction: Direction,
    amount: i32
}

impl FromStr for Command {
    type Err = String;
    fn from_str(line: &str) -> Result<Command, Self::Err> {
        let tokens: Vec<&str> = line.trim().split(" ").collect();
        if tokens.len() != 2 {
            return Err(format!("Could not parse '{}', expected 2 tokens, got {}", line, tokens.len()))
        } else {
            let direction = Direction::from_str(tokens[0])?;
            // TODO figure out how to convert i32 parsing result error without panics
            let amount = tokens[1].parse::<i32>().expect("Error parsing amount");
            return Ok(Command {
                direction,
                amount
            })
        }
    }
}

fn parse_commands(raw: &str) -> Vec<Command> {
    return raw.trim().split("\n").map(|raw_line| {
        Command::from_str(raw_line).expect(format!("Error parsing line '{raw_line}'").as_str())
    }).collect();
}

fn part1(commands: &Vec<Command>) {
    let mut hpos = 0;
    let mut depth = 0;

    for command in commands {
        if command.direction == Direction::Forward {
            hpos += command.amount;
        } else {
            depth += if command.direction == Direction::Down { command.amount } else { -command.amount };
        }
    }

    println!("Part 1: {}", hpos * depth);
}

fn part2(commands: &Vec<Command>) {
    let mut hpos = 0;
    let mut depth = 0;
    let mut aim = 0;

    for command in commands {
        if command.direction == Direction::Forward {
            hpos += command.amount;
            depth += aim * command.amount;
        } else {
            aim += if command.direction == Direction::Down { command.amount } else { -command.amount };
        }
    }

    println!("Part 2: {}", hpos * depth);
}

fn main() {
    let raw = fs::read_to_string(INPUT_PATH)
        .expect("Error reading file");
    let commands = parse_commands(&raw);
    part1(&commands);
    part2(&commands);
}