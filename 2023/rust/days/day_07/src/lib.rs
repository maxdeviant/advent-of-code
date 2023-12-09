use std::cmp::Ordering;

use adventurous::Input;
use anyhow::{anyhow, bail, Result};
use indexmap::IndexMap;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
enum Card {
    Ace = 14,
    King = 13,
    Queen = 12,
    Jack = 11,
    Ten = 10,
    Nine = 9,
    Eight = 8,
    Seven = 7,
    Six = 6,
    Five = 5,
    Four = 4,
    Three = 3,
    Two = 2,
    Joker = 1,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
enum JMeans {
    Jack,
    Joker,
}

impl Card {
    pub fn parse(char: char, j_means: JMeans) -> Result<Self> {
        match char {
            'A' => Ok(Self::Ace),
            'K' => Ok(Self::King),
            'Q' => Ok(Self::Queen),
            'J' => Ok(match j_means {
                JMeans::Jack => Self::Jack,
                JMeans::Joker => Self::Joker,
            }),
            'T' => Ok(Self::Ten),
            '9' => Ok(Self::Nine),
            '8' => Ok(Self::Eight),
            '7' => Ok(Self::Seven),
            '6' => Ok(Self::Six),
            '5' => Ok(Self::Five),
            '4' => Ok(Self::Four),
            '3' => Ok(Self::Three),
            '2' => Ok(Self::Two),
            _ => Err(anyhow!("Invalid card: '{char}'")),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
enum HandType {
    FiveOfAKind = 6,
    FourOfAKind = 5,
    FullHouse = 4,
    ThreeOfAKind = 3,
    TwoPair = 2,
    OnePair = 1,
    HighCard = 0,
}

#[derive(Debug, PartialEq, Eq)]
struct Hand {
    cards: [Card; Self::HAND_SIZE],
}

impl Hand {
    const HAND_SIZE: usize = 5;

    pub fn parse_with_bid(input: &str, j_means: JMeans) -> Result<(Self, usize)> {
        let mut parts = input.split(' ');

        let cards = parts
            .next()
            .ok_or_else(|| anyhow!("missing list of cards"))?;
        let bid = parts.next().ok_or_else(|| anyhow!("missing bid"))?;

        let cards = cards
            .trim()
            .chars()
            .map(|char| Card::parse(char, j_means))
            .collect::<Result<Vec<_>, _>>()?;
        let bid = bid.trim().parse::<usize>()?;

        if cards.len() > Self::HAND_SIZE {
            bail!("hand has more than {} cards", Self::HAND_SIZE);
        }

        let cards = array_init::from_iter(cards.iter().copied()).unwrap();

        Ok((Self { cards }, bid))
    }

    pub fn ty(&self) -> HandType {
        let mut card_counts = IndexMap::new();

        for card in &self.cards {
            *card_counts.entry(card).or_insert(0) += 1;
        }

        card_counts.sort_by(|_, a, _, b| b.cmp(a));

        let mut jokers = card_counts.get(&Card::Joker).copied().unwrap_or(0);
        let mut three_of_a_kind = false;
        let mut pairs = 0;

        for card in card_counts.keys() {
            let Some(count) = card_counts.get(card).copied() else {
                continue;
            };

            let usable_jokers = match card {
                Card::Joker => 0,
                _ => jokers,
            };

            if count + usable_jokers >= 5 {
                return HandType::FiveOfAKind;
            }

            if count + usable_jokers >= 4 {
                return HandType::FourOfAKind;
            }

            if count + usable_jokers >= 3 {
                let jokers_needed = 3 - count;
                jokers -= jokers_needed;

                three_of_a_kind = true;

                continue;
            }

            if count + usable_jokers >= 2 {
                let jokers_needed = 2 - count;
                jokers -= jokers_needed;

                pairs += 1;

                continue;
            }
        }

        match (three_of_a_kind, pairs) {
            (true, 1) => HandType::FullHouse,
            (true, _) => HandType::ThreeOfAKind,
            (false, 2) => HandType::TwoPair,
            (false, 1) => HandType::OnePair,
            _ => HandType::HighCard,
        }
    }
}

impl PartialOrd for Hand {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Hand {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.ty().cmp(&other.ty()) {
            Ordering::Equal => {
                for i in 0..Self::HAND_SIZE {
                    match self.cards[i].cmp(&other.cards[i]) {
                        Ordering::Equal => continue,
                        ord => return ord,
                    }
                }

                Ordering::Equal
            }
            ord => ord,
        }
    }
}

#[adventurous::part_one(answer = "248453531")]
pub fn part_one(input: &Input) -> Result<usize> {
    let hands = input
        .traverse(|line| Hand::parse_with_bid(line, JMeans::Jack))?
        .collect::<Vec<_>>();

    let mut ranked_hands = hands;
    ranked_hands.sort_by(|(a, _), (b, _)| a.cmp(b));

    Ok(ranked_hands
        .into_iter()
        .enumerate()
        .map(|(index, pair)| (index + 1, pair))
        .map(|(rank, (_, bid))| bid * rank)
        .sum())
}

// Not: 248834240
#[adventurous::part_two]
pub fn part_two(input: &Input) -> Result<usize> {
    let hands = input
        .traverse(|line| Hand::parse_with_bid(line, JMeans::Joker))?
        .collect::<Vec<_>>();

    let mut ranked_hands = hands;
    ranked_hands.sort_by(|(a, _), (b, _)| a.cmp(b));

    Ok(ranked_hands
        .into_iter()
        .enumerate()
        .map(|(index, pair)| (index + 1, pair))
        .map(|(rank, (_, bid))| bid * rank)
        .sum())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    use super::*;

    adventurous::test_solutions!();

    #[test]
    fn test_hand_comparison() {
        let hand_one = Hand {
            cards: [Card::King, Card::King, Card::Six, Card::Seven, Card::Seven],
        };
        let hand_two = Hand {
            cards: [Card::King, Card::Ten, Card::Jack, Card::Jack, Card::Ten],
        };

        assert!(hand_one > hand_two);
    }

    #[test]
    fn test_part_one_sample_input() -> Result<()> {
        let input = indoc! {"
            32T3K 765
            T55J5 684
            KK677 28
            KTJJT 220
            QQQJA 483
        "};

        assert_eq!(part_one(&Input::new(input.to_string()))?, 6440);

        Ok(())
    }

    #[test]
    fn test_part_two_sample_input() -> Result<()> {
        let input = indoc! {"
            32T3K 765
            T55J5 684
            KK677 28
            KTJJT 220
            QQQJA 483
        "};

        assert_eq!(part_two(&Input::new(input.to_string()))?, 5905);

        Ok(())
    }
}
