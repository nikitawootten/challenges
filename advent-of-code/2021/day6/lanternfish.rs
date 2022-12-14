use std::str::FromStr;

#[derive(Clone)]
pub struct School {
    /// Day counter starts at 0, resets after 8
    i: usize,
    /// Number of fish per slot
    fish: [u64; 9],
}

impl FromStr for School {
    type Err = String;
    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let mut fish = [0 as u64; 9];
        input.trim().split(",").for_each(|token| {
            let slot = token.parse::<usize>().expect("error parsing fish counter");
            if slot > 8 {
                panic!("fish counter above max value");
            }
            fish[slot] += 1;
        });

        return Ok(Self {
            fish,
            i: 0,
        })
    }
}

impl School {
    pub fn population(&self) -> u64 {
        let mut sum = 0;
        
        for i in self.fish {
            sum += i;
        }

        return sum;
    }

    pub fn step(&mut self) {
        self.fish[(self.i + 7) % 9] += self.fish[self.i];
        self.i = (self.i + 1) % 9;
    }

    pub fn simulate_days(&mut self, days: usize) -> u64 {
        for _ in 1..=days {
            self.step();
        }
        return self.population()
    }
}
