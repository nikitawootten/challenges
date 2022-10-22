use std::fs;

const INPUT_PATH: &str = "day1/input.txt";

fn part1(measurements: &[i32]) {
    let mut measurements_iter = measurements.iter();
    
    let mut num_increased = 0;
    let mut prev_measurement = measurements_iter.next().expect("Require at least 1 measurement");
    
    for measurement in measurements_iter {
        if measurement > prev_measurement {
            num_increased += 1;
        }
        prev_measurement = measurement;
    }

    println!("Part 1: {num_increased}");
}

const WINDOW_SIZE: usize = 3;

fn wrap_around(i: i32) -> usize {
    if i < 0 {
        return wrap_around(i + WINDOW_SIZE as i32 + 1);
    } else if i >= WINDOW_SIZE as i32 + 1 {
        return wrap_around(i - WINDOW_SIZE as i32 - 1);
    } else {
        return i as usize;
    }
}

fn part2(measurements: &[i32]) {
    let mut num_increased = 0;
    let mut sums = [0; WINDOW_SIZE + 1];
    let mut window_i = 0;

    // create some mechanism for ignoring the first WINDOW_SIZE + 1 sums
    let mut ignore_steps = 0;

    for measurement in measurements.iter() { 
        for i in 0..WINDOW_SIZE {
           sums[wrap_around(window_i as i32 - i as i32)] += measurement;
        }

        // compare finished sum (window_i - 2) and previous sum (window_i - 3)
        if ignore_steps >= WINDOW_SIZE && sums[wrap_around(window_i as i32 - 2)] > sums[wrap_around(window_i as i32 - 3)] {
            num_increased += 1;
        }

        sums[wrap_around(window_i as i32 + 1)] = 0;
        ignore_steps += 1;
        window_i = wrap_around(window_i as i32 + 1);
    }

    println!("Part 2: {num_increased}");
}

fn main() {
    let raw = fs::read_to_string(INPUT_PATH)
        .expect("Error reading file");
    let measurements = raw.trim().split("\n")
        .map(|raw_line| {
            raw_line.parse::<i32>()
                .expect(format!("Error parsing line '{raw_line}' into struct").as_str())
        }).collect::<Vec<i32>>();

    part1(&measurements);
    part2(&measurements);
}