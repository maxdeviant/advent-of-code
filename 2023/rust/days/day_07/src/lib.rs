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

#[derive(Debug)]
struct CardCounts {
    counts: IndexMap<Card, i32>,
    jokers: i32,
}

impl CardCounts {
    pub fn new(cards: impl Iterator<Item = Card>) -> Self {
        let mut counts = IndexMap::new();

        for card in cards {
            *counts.entry(card).or_insert(0) += 1;
        }

        let jokers = counts.get(&Card::Joker).copied().unwrap_or(0);

        Self { counts, jokers }
    }

    pub fn five_of_a_kind(&self) -> Option<Self> {
        self.n_of_a_kind(5)
    }

    pub fn four_of_a_kind(&self) -> Option<Self> {
        self.n_of_a_kind(4)
    }

    pub fn full_house(&self) -> Option<Self> {
        self.three_of_a_kind()?.one_pair()
    }

    pub fn three_of_a_kind(&self) -> Option<Self> {
        self.n_of_a_kind(3)
    }

    pub fn two_pair(&self) -> Option<Self> {
        self.one_pair()?.one_pair()
    }

    pub fn one_pair(&self) -> Option<Self> {
        self.n_of_a_kind(2)
    }

    fn n_of_a_kind(&self, n: i32) -> Option<Self> {
        let mut jokers = self.jokers;

        for (card, count) in &self.counts {
            if count + jokers >= n {
                let jokers_needed = n - count;
                jokers -= jokers_needed;

                let mut counts = self.counts.clone();
                *counts.get_mut(card).unwrap() -= n - jokers_needed;
                if let Some(joker_count) = counts.get_mut(&Card::Joker) {
                    *joker_count -= jokers_needed;
                }

                return Some(Self { counts, jokers });
            }
        }

        None
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
        let counts = CardCounts::new(self.cards.iter().copied());

        if counts.five_of_a_kind().is_some() {
            return HandType::FiveOfAKind;
        }

        if counts.four_of_a_kind().is_some() {
            return HandType::FourOfAKind;
        }

        if counts.full_house().is_some() {
            return HandType::FullHouse;
        }

        if counts.three_of_a_kind().is_some() {
            return HandType::ThreeOfAKind;
        }

        if counts.two_pair().is_some() {
            return HandType::TwoPair;
        }

        if counts.one_pair().is_some() {
            return HandType::OnePair;
        }

        HandType::HighCard
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

// Not: 249299342 (too high)
// Not: 249050660
// Not: 248834240 (too high)
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
    fn test_hand_type_with_all_jokers() {
        let hand = Hand {
            cards: [
                Card::Joker,
                Card::Joker,
                Card::Joker,
                Card::Joker,
                Card::Joker,
            ],
        };

        assert_eq!(hand.ty(), HandType::FiveOfAKind);
    }

    #[test]
    fn test_hand_type_with_four_of_a_kind_and_one_joker() {
        let hand = Hand {
            cards: [
                Card::Queen,
                Card::Queen,
                Card::Joker,
                Card::Queen,
                Card::Queen,
            ],
        };

        assert_eq!(hand.ty(), HandType::FiveOfAKind);
    }

    #[test]
    fn test_hand_type_with_two_pair_and_three_jokers() {
        let hand = Hand {
            cards: [Card::Two, Card::Joker, Card::Joker, Card::Two, Card::Joker],
        };

        assert_eq!(hand.ty(), HandType::FiveOfAKind);
    }

    #[test]
    fn test_hand_type_with_two_pair_one_card_and_two_jokers() {
        let hand = Hand {
            cards: [Card::Two, Card::Joker, Card::Joker, Card::Two, Card::Queen],
        };

        assert_eq!(hand.ty(), HandType::FourOfAKind);
    }

    #[test]
    fn test_hand_type_with_two_pair_two_pair_and_one_joker() {
        let hand = Hand {
            cards: [Card::Two, Card::Joker, Card::Seven, Card::Two, Card::Seven],
        };

        assert_eq!(hand.ty(), HandType::FullHouse);
    }
}
