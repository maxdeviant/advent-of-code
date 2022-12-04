use std::collections::HashSet;
use std::num::ParseIntError;
use std::str::FromStr;

use adventurous::Input;
use anyhow::Result;
use thiserror::Error;

#[derive(Debug)]
struct SectionAssignment(i32, i32);

impl SectionAssignment {
    pub fn fully_contains(&self, other: &Self) -> bool {
        self.0 <= other.0 && self.1 >= other.1
    }

    pub fn overlaps_with(&self, other: &Self) -> bool {
        let first_assignment: HashSet<i32> = HashSet::from_iter(self.0..=self.1);
        let second_assignment = HashSet::from_iter(other.0..=other.1);

        let overlap: HashSet<_> = first_assignment.intersection(&second_assignment).collect();

        !overlap.is_empty()
    }
}

#[derive(Error, Debug)]
enum ParseSectionAssignmentError {
    #[error("invalid section assignment input: {0}")]
    InvalidInput(String),

    #[error("failed to parse section: {0}")]
    ParseIntError(#[from] ParseIntError),
}

impl FromStr for SectionAssignment {
    type Err = ParseSectionAssignmentError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.split('-').collect::<Vec<_>>().as_slice() {
            [start, end] => {
                let start: i32 = start.parse()?;
                let end: i32 = end.parse()?;

                Ok(Self(start, end))
            }
            _ => Err(ParseSectionAssignmentError::InvalidInput(s.to_string())),
        }
    }
}

#[derive(Debug)]
struct SectionAssignmentPair(SectionAssignment, SectionAssignment);

impl FromStr for SectionAssignmentPair {
    type Err = ParseSectionAssignmentError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.split(',').collect::<Vec<_>>().as_slice() {
            [first_assignment, second_assignment] => {
                let first_assignment: SectionAssignment = first_assignment.parse()?;
                let second_assignment: SectionAssignment = second_assignment.parse()?;

                Ok(Self(first_assignment, second_assignment))
            }
            _ => Err(ParseSectionAssignmentError::InvalidInput(s.to_string())),
        }
    }
}

fn part_one(input: &Input) -> Result<usize> {
    let section_assignment_pairs = input
        .value
        .lines()
        .map(|line| line.parse::<SectionAssignmentPair>())
        .collect::<Result<Vec<_>, _>>()?;

    Ok(section_assignment_pairs
        .iter()
        .filter(|pair| pair.0.fully_contains(&pair.1) || pair.1.fully_contains(&pair.0))
        .count())
}

fn part_two(input: &Input) -> Result<usize> {
    let section_assignment_pairs = input
        .value
        .lines()
        .map(|line| line.parse::<SectionAssignmentPair>())
        .collect::<Result<Vec<_>, _>>()?;

    Ok(section_assignment_pairs
        .iter()
        .filter(|pair| pair.0.overlaps_with(&pair.1))
        .count())
}

fn main() -> Result<()> {
    let input = Input::from_file("input.txt")?;

    println!("Part One: {}", part_one(&input)?);
    println!("Part Two: {}", part_two(&input)?);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one_solution() -> Result<()> {
        let input = Input::from_file("input.txt")?;

        Ok(assert_eq!(part_one(&input)?, 651))
    }

    #[test]
    fn test_part_two_solution() -> Result<()> {
        let input = Input::from_file("input.txt")?;

        Ok(assert_eq!(part_two(&input)?, 956))
    }

    #[test]
    fn test_part_one_sample_input() -> Result<()> {
        let input = Input {
            value: r#"
2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8
            "#
            .trim()
            .to_string(),
        };

        Ok(assert_eq!(part_one(&input)?, 2))
    }

    #[test]
    fn test_part_two_sample_input() -> Result<()> {
        let input = Input {
            value: r#"
2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8
            "#
            .trim()
            .to_string(),
        };

        Ok(assert_eq!(part_two(&input)?, 4))
    }
}
