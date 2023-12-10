use std::collections::VecDeque;

use adventurous::Input;
use anyhow::Result;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub enum ExtrapolationDirection {
    Forwards,
    Backwards,
}

use ExtrapolationDirection::*;

#[derive(Debug, Clone)]
pub struct History(Vec<i32>);

impl History {
    pub fn parse(input: &str) -> Result<Self> {
        Ok(Self(
            input
                .split(' ')
                .map(|str| str.parse::<i32>())
                .collect::<Result<Vec<_>, _>>()?,
        ))
    }

    pub fn extrapolate(&self, direction: ExtrapolationDirection) -> i32 {
        let mut current_sequence = self.0.clone();
        let mut sequences = VecDeque::from_iter([VecDeque::from_iter(current_sequence.clone())]);

        loop {
            current_sequence = differences(&current_sequence);
            sequences.push_back(VecDeque::from_iter(current_sequence.clone()));

            if current_sequence.iter().all(|n| *n == 0) {
                break;
            }
        }

        let mut below = 0;

        for sequence in sequences.iter_mut().rev() {
            match direction {
                Forwards => {
                    let left = sequence.back().unwrap();
                    sequence.push_back(left + below);
                    below = *sequence.back().unwrap();
                }
                Backwards => {
                    let right = sequence.front().unwrap();
                    sequence.push_front(right - below);
                    below = *sequence.front().unwrap();
                }
            }
        }

        let first_sequence = sequences.front().unwrap();

        match direction {
            Forwards => *first_sequence.back().unwrap(),
            Backwards => *first_sequence.front().unwrap(),
        }
    }
}

fn differences(sequence: &[i32]) -> Vec<i32> {
    let mut differences = Vec::new();

    let Some((mut x, rest)) = sequence.split_first() else {
        return differences;
    };

    for y in rest {
        let diff = y - x;

        differences.push(diff);

        x = y;
    }

    differences
}

#[adventurous::part_one(answer = "1934898178")]
pub fn part_one(input: &Input) -> Result<i32> {
    Ok(input
        .traverse(History::parse)?
        .map(|history| history.extrapolate(Forwards))
        .sum())
}

#[adventurous::part_two(answer = "1129")]
pub fn part_two(input: &Input) -> Result<i32> {
    Ok(input
        .traverse(History::parse)?
        .map(|history| history.extrapolate(Backwards))
        .sum())
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
            0 3 6 9 12 15
            1 3 6 10 15 21
            10 13 16 21 30 45
        "};

        assert_eq!(part_one(&Input::new(input.to_string()))?, 114);

        Ok(())
    }

    #[test]
    fn test_part_two_sample_input() -> Result<()> {
        let input = indoc! {"
            0 3 6 9 12 15
            1 3 6 10 15 21
            10 13 16 21 30 45
        "};

        assert_eq!(part_two(&Input::new(input.to_string()))?, 2);

        Ok(())
    }
}
