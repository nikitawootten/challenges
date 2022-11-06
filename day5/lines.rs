use std::str::FromStr;
use std::fmt::Display;

#[derive(Clone, Copy, Hash, Eq, PartialEq, Debug)]
pub struct Coordinate(u32, u32);

impl FromStr for Coordinate {
    type Err = String;
    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let pieces = input.trim().split(",")
            .map(|raw| raw.trim().parse::<u32>().expect("coordinate piece not valid"))
            .collect::<Vec<u32>>();
        
        if pieces.len() != 2 {
            return Err(format!("'{input}' contained an incorrect number of coordinate pieces"))
        }

        return Ok(Self(pieces[0], pieces[1]));
    }
}

impl Display for Coordinate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.0, self.1)
    }
}

pub struct Line(Coordinate, Coordinate);

impl FromStr for Line {
    type Err = String;
    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let coordinates = input.trim().split(" -> ")
            .map(|raw| raw.trim().parse::<Coordinate>().expect("coordinate not valid"))
            .collect::<Vec<Coordinate>>();
        
        if coordinates.len() != 2 {
            return Err(format!("'{input}' contained an incorrect number of coordinate pieces"))
        }

        return Ok(Self(coordinates[0], coordinates[1]));
    }
}

impl Display for Line {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} -> {}", self.0, self.1)
    }
}

impl Line {
    /// Return all points along this line
    pub fn points(&self) -> Vec<Coordinate> {
        if self.0.0 == self.1.0 {
            // horizontal line
            if self.0.1 < self.1.1 {
                return (self.0.1..=self.1.1).map(|y| Coordinate(self.0.0, y)).collect();
            } else {
                return (self.1.1..=self.0.1).rev().map(|y| Coordinate(self.0.0, y)).collect();
            }
        } else if self.0.1 == self.1.1 {
            // vertical line
            if self.0.0 < self.1.0 {
                return (self.0.0..=self.1.0).map(|x| Coordinate(x, self.0.1)).collect();
            } else {
                return (self.1.0..=self.0.0).rev().map(|x| Coordinate(x, self.0.1)).collect();
            }
        } else {
            return Vec::from([]);
        }
    }

    /// Returns all points along this line, including diagonals
    pub fn points_diag(&self) -> Vec<Coordinate> {
        if self.0.0 == self.1.0 || self.0.1 == self.1.1 {
            return self.points();
        } else if (self.0.0 as i64 - self.1.0 as i64).abs() == (self.0.1 as i64 - self.1.1 as i64).abs() {
            // diagonal line
            if self.0.0 < self.1.0 {
                if self.0.1 < self.1.1 {
                    return (0..=(self.1.0 - self.0.0))
                        .map(|i| Coordinate(self.0.0 + i, self.0.1 + i)).collect();
                } else {
                    return (0..=(self.1.0 - self.0.0))
                        .map(|i| Coordinate(self.0.0 + i, self.0.1 - i)).collect();
                }
            } else {
                if self.0.1 < self.1.1 {
                    return (0..=(self.0.0 - self.1.0))
                        .map(|i| Coordinate(self.0.0 - i, self.0.1 + i)).collect();
                } else {
                    return (0..=(self.0.0 - self.1.0))
                        .map(|i| Coordinate(self.0.0 - i, self.0.1 - i)).collect();
                }
            }
        } else {
            return Vec::from([]);
        }
    }
}

#[cfg(test)]
mod tests {
    // Import names from outer "lines" scope (via https://doc.rust-lang.org/rust-by-example/testing/unit_testing.html)
    use super::*;

    #[test]
    fn test_line_points() {
        let example1 = Line::from_str("1,1 -> 1,3").unwrap();
        assert_eq!(example1.points(), Vec::from([
            Coordinate(1, 1),
            Coordinate(1, 2),
            Coordinate(1, 3)
        ]));

        let example2 = Line::from_str("9,7 -> 7,7").unwrap();
        assert_eq!(example2.points(), Vec::from([
            Coordinate(9, 7),
            Coordinate(8, 7),
            Coordinate(7, 7),
        ]));
    }

    #[test]
    fn test_line_points_diag() {
        let example1 = Line::from_str("1,1 -> 3,3").unwrap();
        assert_eq!(example1.points_diag(), Vec::from([
            Coordinate(1, 1),
            Coordinate(2, 2),
            Coordinate(3, 3),
        ]));

        let example2 = Line::from_str("9,7 -> 7,9").unwrap();
        assert_eq!(example2.points_diag(), Vec::from([
            Coordinate(9, 7),
            Coordinate(8, 8),
            Coordinate(7, 9),
        ]));
    }
}
