use std::str::FromStr;

pub const BOARD_SIZE: usize = 10;
const MAX_ENERGY: u8 = 9;

pub struct Board {
    /// the grid of octopi, represented by their energy levels
    grid: [[u8; BOARD_SIZE]; BOARD_SIZE],
    /// the number of steps that have been simulated
    pub steps: u32,
    /// the number of flashes that have occurred
    pub flashes: u32,
}

impl FromStr for Board {
    type Err = String;
    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let mut grid = [[0 as u8; BOARD_SIZE]; BOARD_SIZE];
        for (i, line) in input.trim().split("\n").enumerate() {
            for (j, token) in line.chars().enumerate() {
                if i > BOARD_SIZE || j > BOARD_SIZE {
                    return Err(format!("position ({i},{j}) is greater then the board size {BOARD_SIZE}"))
                }
                let parsed_token = match token.to_string().parse::<u8>() {
                    Ok(t) => t,
                    Err(e) => return Err(e.to_string())
                };
                if parsed_token > 9 {
                    return Err(format!("energy level at ({i}{j}) is invalid: '{parsed_token}'"))
                }

                grid[i][j] = parsed_token;
            }
        }

        return Ok(Self {
            grid: grid,
            steps: 0,
            flashes: 0
        })
    }
}

impl Board {
    pub fn step(&mut self) {
        self.steps += 1;
        
        let mut flash_queue: Vec<(usize, usize)> = Vec::new();

        for i in 0..BOARD_SIZE {
            for j in 0..BOARD_SIZE {
                self.grid[i][j] += 1;

                if self.grid[i][j] > MAX_ENERGY {
                    flash_queue.push((i, j));
                }
            }
        }

        for (i, j) in flash_queue {
            if self.grid[i][j] > 0 {
                self.flash(i, j);
            }
        }
    }

    pub fn flash(&mut self, i: usize, j: usize) {
        self.flashes += 1;
        self.grid[i][j] = 0;
        
        let mut neighbor_queue: Vec<(usize, usize)> = Vec::new();

        if i > 0 {
            neighbor_queue.push((i - 1, j));
            if j > 0 {
                neighbor_queue.push((i - 1, j - 1));
            }
            if j < BOARD_SIZE - 1 {
                neighbor_queue.push((i - 1, j + 1));
            }
        }

        if i < BOARD_SIZE - 1 {
            neighbor_queue.push((i + 1, j));
            if j > 0 {
                neighbor_queue.push((i + 1, j - 1));
            }
            if j < BOARD_SIZE - 1 {
                neighbor_queue.push((i + 1, j + 1));
            }
        }

        if j > 0 {
            neighbor_queue.push((i, j - 1));
        }

        if j < BOARD_SIZE - 1 {
            neighbor_queue.push((i, j + 1));
        }

        for (adj_i, adj_j) in neighbor_queue {
            if self.grid[adj_i][adj_j] > 0 {
                self.grid[adj_i][adj_j] += 1;
                if self.grid[adj_i][adj_j] > MAX_ENERGY {
                    self.flash(adj_i, adj_j)
                }
            }
        }
    }
}
