use std::num::ParseIntError;

use adventurous::Input;
use anyhow::{anyhow, Result};

#[derive(Debug)]
struct Race {
    pub time: usize,
    pub record_distance: usize,
}

impl Race {
    fn ways_to_win(&self) -> usize {
        (1..self.time)
            .filter_map(|held_seconds| {
                let seconds_of_movement = self.time - held_seconds;
                let speed = held_seconds;

                let distance = speed * seconds_of_movement;

                if distance > self.record_distance {
                    Some(held_seconds)
                } else {
                    None
                }
            })
            .count()
    }
}

fn parse_races(input: &Input) -> Result<Vec<Race>> {
    fn parse_numbers(input: &str) -> Result<Vec<usize>, ParseIntError> {
        input
            .split(" ")
            .map(|str| str.trim())
            .filter(|str| !str.is_empty())
            .map(|str| str.parse::<usize>())
            .collect()
    }

    let mut lines = input.lines();
    let times = lines.next().ok_or_else(|| anyhow!("missing times"))?;
    let distances = lines.next().ok_or_else(|| anyhow!("missing distances"))?;

    let times = parse_numbers(&times.replace("Time:", ""))?;
    let distances = parse_numbers(&distances.replace("Distance:", ""))?;

    Ok(times
        .into_iter()
        .zip(distances)
        .map(|(time, record_distance)| Race {
            time,
            record_distance,
        })
        .collect())
}

#[adventurous::part_one(answer = "440000")]
pub fn part_one(input: &Input) -> Result<usize> {
    let races = parse_races(input)?;

    Ok(races
        .into_iter()
        .map(|race| race.ways_to_win())
        .fold(1, |acc, ways| acc * ways))
}

#[adventurous::part_two]
pub fn part_two(input: &Input) -> Result<usize> {
    todo!()
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    use super::*;

    adventurous::test_solutions!();

    #[test]
    fn test_part_one_sample_input() -> Result<()> {
        let input = indoc! {"
            Time:      7  15   30
            Distance:  9  40  200
        "};

        assert_eq!(part_one(&Input::new(input.to_string()))?, 288);

        Ok(())
    }
}
