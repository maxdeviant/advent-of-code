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
    pub fn unique_items(&self) -> HashSet<&Item> {
        HashSet::from_iter(
            self.first_compartment
                .iter()
                .chain(self.second_compartment.iter()),
        )
    }

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

#[derive(Debug, Clone, Copy)]
struct Badge(Item);

impl From<Badge> for Item {
    fn from(badge: Badge) -> Self {
        badge.0
    }
}

struct Group<'a> {
    elves: &'a [Rucksack; 3],
}

impl<'a> Group<'a> {
    pub fn new(rucksacks: &'a [Rucksack]) -> Self {
        Self {
            elves: rucksacks.try_into().unwrap(),
        }
    }

    pub fn badge(&self) -> Option<Badge> {
        let x = self.elves[0].unique_items();
        let y = self.elves[1].unique_items();
        let z = self.elves[2].unique_items();

        let x_intersect_y: HashSet<_> = x.intersection(&y).map(|item| *item).collect();
        let xy_intersect_z: HashSet<_> = x_intersect_y.intersection(&z).map(|item| *item).collect();

        xy_intersect_z
            .iter()
            .next()
            .cloned()
            .map(|item| Badge(*item))
    }
}

fn part_two(input: &Input) -> usize {
    let priorities = ItemPriorities::new();

    let rucksacks = input
        .value
        .lines()
        .map(|line| line.parse::<Rucksack>())
        .collect::<Result<Vec<_>, _>>()
        .expect("failed to parse rucksacks");

    rucksacks
        .chunks(3)
        .map(Group::new)
        .map(|group| group.badge().expect("failed to get badge for group"))
        .map(|badge| {
            priorities
                .get_priority(&badge.into())
                .expect(&format!("no priority found for {:?}", badge))
        })
        .sum()
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
    fn test_part_two_solution() -> std::io::Result<()> {
        let input = Input::from_file("input.txt")?;

        Ok(assert_eq!(part_two(&input), 2577))
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
