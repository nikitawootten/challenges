use std::{fs, str::FromStr};

mod octopus;
use octopus::{Board, BOARD_SIZE};

const INPUT_PATH: &str = "day11/input.txt";

fn main() {
    let input = fs::read_to_string(INPUT_PATH).expect("error reading file");
    let mut board = Board::from_str(&input).expect("error parsing board");

    let mut prev_flashes = 0;
    loop {
        board.step();

        if board.steps == 100 {
            println!("Part 1: {}", board.flashes);
        }

        if board.flashes - prev_flashes == (BOARD_SIZE * BOARD_SIZE) as u32 {
            println!("Part 2: {}", board.steps);
            break;
        }
        
        prev_flashes = board.flashes;
    }
}
