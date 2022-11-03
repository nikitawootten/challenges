use std::str::FromStr;

pub type BingoNumber = u32;
const BOARD_SIZE: usize = 5;

#[derive(Clone, Copy)]
pub struct Board {
    board: [[BingoNumber; BOARD_SIZE]; BOARD_SIZE],
    marked: [[bool; BOARD_SIZE]; BOARD_SIZE],
    pub won: bool
}

impl Board {
    /// Returns a score if the call winds the game
    pub fn call(&mut self, number: &BingoNumber) -> Option<BingoNumber> {
        if self.won {
            panic!("'call' method called on board that has already won");
        }

        for i in 0..BOARD_SIZE {
            for j in 0..BOARD_SIZE {
                if self.board[i][j] == *number {
                    self.marked[i][j] = true;

                    if self.check_won(i, j) {
                        self.won = true;
                        return Some(number * self.unmarked_sum());
                    }
                }
            }
        }

        None
    }

    fn check_won(&self, row: usize, col: usize) -> bool {
        let mut row_wins = true;
        let mut col_wins = true;
        for i in 0..BOARD_SIZE {
            if !self.marked[row][i] {
                col_wins = false;
            }
            if !self.marked[i][col] {
                row_wins = false;
            }
        }

        return col_wins || row_wins;
    }

    fn unmarked_sum(&self) -> BingoNumber {
        let mut sum = 0;
        for i in 0..BOARD_SIZE {
            for j in 0..BOARD_SIZE {
                if !self.marked[i][j] {
                    sum += self.board[i][j];
                }
            }
        }
        return sum;
    }
}

impl FromStr for Board {
    type Err = String;
    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let mut board = [[0 as BingoNumber; BOARD_SIZE]; BOARD_SIZE];

        for (i, row) in input.trim().split("\n").enumerate() {
            if i >= BOARD_SIZE {
                panic!("expected board of {BOARD_SIZE} rows, got at least {i}");
            }
            
            for (j, raw_cell) in row.split_ascii_whitespace().enumerate() {
                if j >= BOARD_SIZE {
                    panic!("expected row of {BOARD_SIZE} cells, got at least {j}");
                }

                board[i][j] = raw_cell.parse::<BingoNumber>()
                    .expect(format!("token '{raw_cell}' could not be parsed into a bingo number").as_str());
            }
        }

        return Ok(Board {
            board,
            marked: [[false; BOARD_SIZE]; BOARD_SIZE],
            won: false,
        });
    }
}

/// Parse a game input, returning the vector of boards and game calls
pub fn parse_game(input: &str) -> (Vec<Board>, Vec<BingoNumber>) {
    let mut stanzas = input.trim().split("\n\n");

    // the first "stanza" corresponds to the vector of bingo calls
    let raw_calls = stanzas.next()
        .expect("input must include at least 2 stanzas");
    let calls = raw_calls.trim().split(",").map(|raw_call| {
        return raw_call.parse::<BingoNumber>()
            .expect(format!("token '{raw_call}' could not be parsed into a bingo number").as_str());
    }).collect();

    // the remaining "stanzas" correspond to board games
    let boards = stanzas.map(|raw_board| {
        return Board::from_str(raw_board).unwrap()
    }).collect();
    
    return (boards, calls);
}