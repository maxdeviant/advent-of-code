use std::collections::{HashMap, HashSet};
use std::str::FromStr;

use adventurous::Input;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Item(char);

#[derive(Debug)]
struct Rucksack {
    first_compartment: Vec<Item>,
    second_compartment: Vec<Item>,
}

impl Rucksack {
    pub fn items_in_both_compartments(&self) -> HashSet<&Item> {
        let mut items = HashSet::new();

        let second_compartment_contents: HashSet<&Item> =
            HashSet::from_iter(self.second_compartment.iter());

        for item in &self.first_compartment {
            if second_compartment_contents.contains(&item) {
                items.insert(item);
            }
        }

        items
    }
}

impl FromStr for Rucksack {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (first_compartment, second_compartment) = s.split_at(s.len() / 2);

        Ok(Self {
            first_compartment: first_compartment.chars().map(Item).collect(),
            second_compartment: second_compartment.chars().map(Item).collect(),
        })
    }
}

#[derive(Debug)]
struct ItemPriorities {
    priorities: HashMap<Item, usize>,
}

impl ItemPriorities {
    pub fn new() -> Self {
        let mut priorities = HashMap::new();

        for (index, char) in ('a'..='z').enumerate() {
            priorities.insert(Item(char), index + 1);
        }

        for (index, char) in ('A'..='Z').enumerate() {
            priorities.insert(Item(char), index + 26 + 1);
        }

        Self { priorities }
    }

    pub fn get_priority(&self, item: &Item) -> Option<usize> {
        self.priorities.get(item).cloned()
    }
}

fn part_one(input: &Input) -> usize {
    let priorities = ItemPriorities::new();

    let rucksacks = input
        .value
        .lines()
        .map(|line| line.parse::<Rucksack>())
        .collect::<Result<Vec<_>, _>>()
        .expect("failed to parse rucksacks");

    rucksacks
        .iter()
        .flat_map(|rucksack| rucksack.items_in_both_compartments())
        .map(|item| {
            priorities
                .get_priority(&item)
                .expect(&format!("no priority found for {:?}", item))
        })
        .sum()
}

fn part_two(input: &Input) -> i32 {
    0
}

fn main() -> std::io::Result<()> {
    let input = Input::from_file("input.txt")?;

    println!("Part One: {}", part_one(&input));
    println!("Part Two: {}", part_two(&input));

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one_solution() -> std::io::Result<()> {
        let input = Input::from_file("input.txt")?;

        Ok(assert_eq!(part_one(&input), 7826))
    }

    #[test]
    #[ignore = "Not yet solved"]
    fn test_part_two_solution() -> std::io::Result<()> {
        let input = Input::from_file("input.txt")?;

        Ok(assert_eq!(part_two(&input), 0))
    }

    #[test]
    fn test_part_one_sample_input() {
        let input = Input {
            value: r#"
vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
            "#
            .trim()
            .to_string(),
        };

        assert_eq!(part_one(&input), 157);
    }
}
