use std::collections::HashSet;

use adventurous::Input;
use anyhow::{anyhow, Result};

struct Card {
    pub winning_numbers: HashSet<usize>,
    pub your_numbers: HashSet<usize>,
}

impl Card {
    pub fn parse(line: &str) -> Result<Self> {
        let mut parts = line.split(':');
        let _card = parts.next().ok_or_else(|| anyhow!("no card identifier"))?;
        let numbers = parts.next().ok_or_else(|| anyhow!("no numbers"))?;

        let mut parts = numbers.split('|');
        let winning_numbers = parts.next().ok_or_else(|| anyhow!("no winning numbers"))?;
        let your_numbers = parts.next().ok_or_else(|| anyhow!("no numbers you have"))?;

        let winning_numbers = winning_numbers
            .split(' ')
            .map(|n| n.trim())
            .filter(|n| !n.is_empty())
            .map(|n| n.parse::<usize>())
            .collect::<Result<HashSet<_>, _>>()?;

        let your_numbers = your_numbers
            .split(' ')
            .map(|n| n.trim())
            .filter(|n| !n.is_empty())
            .map(|n| n.parse::<usize>())
            .collect::<Result<HashSet<_>, _>>()?;

        Ok(Self {
            winning_numbers,
            your_numbers,
        })
    }

    pub fn value(&self) -> usize {
        let mut value = 0;

        for winning_number in &self.winning_numbers {
            if self.your_numbers.contains(&winning_number) {
                if value == 0 {
                    value = 1;
                } else {
                    value *= 2;
                }
            }
        }

        value
    }
}

#[adventurous::part_one(answer = "25174")]
pub fn part_one(input: &Input) -> Result<usize> {
    Ok(input.traverse(Card::parse)?.map(|card| card.value()).sum())
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
            Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
            Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
            Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
            Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
            Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
            Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
        "};

        assert_eq!(part_one(&Input::new(input.to_string()))?, 13);

        Ok(())
    }
}
