use std::collections::{HashMap, HashSet};

use adventurous::Input;
use anyhow::{anyhow, Result};
use indexmap::IndexMap;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
struct CardId(usize);

struct Card {
    pub id: CardId,
    pub winning_numbers: HashSet<usize>,
    pub your_numbers: HashSet<usize>,
}

impl Card {
    pub fn parse(line: &str) -> Result<Self> {
        let mut parts = line.split(':');
        let card = parts.next().ok_or_else(|| anyhow!("no card identifier"))?;
        let numbers = parts.next().ok_or_else(|| anyhow!("no numbers"))?;

        let mut parts = numbers.split('|');
        let winning_numbers = parts.next().ok_or_else(|| anyhow!("no winning numbers"))?;
        let your_numbers = parts.next().ok_or_else(|| anyhow!("no numbers you have"))?;

        let id = CardId(card.replace("Card", "").trim().parse::<usize>()?);

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
            id,
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

    pub fn matches(&self) -> usize {
        self.winning_numbers
            .intersection(&self.your_numbers)
            .count()
    }
}

fn count_copies(
    copies_by_card: &mut IndexMap<CardId, usize>,
    matches_by_card: &HashMap<CardId, usize>,
    max_card_id: CardId,
    card_id: CardId,
) -> usize {
    let matches = *matches_by_card.get(&card_id).unwrap();

    let mut total_copies = 0;

    for i in 1..=matches {
        let copy_id = CardId(card_id.0 + i);
        if copy_id > max_card_id {
            break;
        }

        let copies = copies_by_card
            .get(&copy_id)
            .copied()
            .unwrap_or_else(|| count_copies(copies_by_card, matches_by_card, max_card_id, copy_id));

        total_copies += copies;
    }

    1 + total_copies
}

#[adventurous::part_one(answer = "25174")]
pub fn part_one(input: &Input) -> Result<usize> {
    Ok(input.traverse(Card::parse)?.map(|card| card.value()).sum())
}

#[adventurous::part_two(answer = "6420979")]
pub fn part_two(input: &Input) -> Result<usize> {
    let cards = input
        .traverse(Card::parse)?
        .map(|card| (card.id, card))
        .collect::<IndexMap<_, _>>();

    let max_card_id = cards
        .keys()
        .max()
        .copied()
        .ok_or_else(|| anyhow!("no max card ID"))?;

    let matches_by_card = cards
        .values()
        .map(|card| (card.id, card.matches()))
        .collect::<HashMap<_, _>>();

    let mut copies_by_card = IndexMap::new();

    for card in cards.values().rev() {
        let copies = count_copies(&mut copies_by_card, &matches_by_card, max_card_id, card.id);

        copies_by_card.insert(card.id, copies);
    }

    Ok(copies_by_card.values().sum())
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

    #[test]
    fn test_part_two_sample_input() -> Result<()> {
        let input = indoc! {"
            Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
            Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
            Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
            Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
            Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
            Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
        "};

        assert_eq!(part_two(&Input::new(input.to_string()))?, 30);

        Ok(())
    }
}
