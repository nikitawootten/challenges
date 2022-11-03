use std::fs;

mod bingo;

use bingo::parse_game;
use bingo::BingoNumber;
use bingo::Board;

const INPUT_PATH: &str = "day4/input.txt";

fn part1(mut boards: Vec<Board>, calls: &Vec<BingoNumber>) {
    for call in calls {
        for board in boards.iter_mut() {
            let result = board.call(call);
            match result {
                Some(score) => println!("Part 1: {score}"),
                None => ()
            }

            if result.is_some() {
                return;
            }
        }
    }
}

fn part2(mut boards: Vec<Board>, calls: &Vec<BingoNumber>) {
    let mut last_score = 0;
    for call in calls {
        for board in boards.iter_mut() {
            match board.call(call) {
                Some(score) => last_score = score,
                None => ()
            }
        }
        boards.retain(|board| !board.won)
    }
    println!("Part 2: {last_score}")
}

fn main() {
    let raw = fs::read_to_string(INPUT_PATH)
        .expect("Error reading file");
    let (boards, calls) = parse_game(&raw);

    part1(boards.clone(), &calls);
    part2(boards, &calls);
}
