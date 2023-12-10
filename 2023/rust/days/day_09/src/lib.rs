use adventurous::Input;
use anyhow::Result;

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

    pub fn extrapolate(&self) -> i32 {
        let mut current_sequence = self.0.clone();
        let mut sequences = vec![current_sequence.clone()];

        loop {
            current_sequence = differences(&current_sequence);
            sequences.push(current_sequence.clone());

            if current_sequence.iter().all(|n| *n == 0) {
                break;
            }
        }

        let mut below = 0;

        for sequence in sequences.iter_mut().rev() {
            let left = sequence.last().unwrap();
            sequence.push(left + below);
            below = *sequence.last().unwrap();
        }

        *sequences.first().unwrap().last().unwrap()
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
        .map(|history| history.extrapolate())
        .sum())
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
            0 3 6 9 12 15
            1 3 6 10 15 21
            10 13 16 21 30 45
        "};

        assert_eq!(part_one(&Input::new(input.to_string()))?, 114);

        Ok(())
    }

    #[test]
    #[ignore = "not yet solved"]
    fn test_part_two_sample_input() -> Result<()> {
        let input = indoc! {"

        "};

        assert_eq!(part_two(&Input::new(input.to_string()))?, 0);

        Ok(())
    }
}
