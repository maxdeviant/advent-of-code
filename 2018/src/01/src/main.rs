extern crate adventurous;

use std::collections::{HashSet, VecDeque};

use adventurous::Input;

#[derive(Debug, Clone)]
enum FrequencyChange {
    Increase(i32),
    Decrease(i32),
}

impl FrequencyChange {
    pub fn from_str(input: &str) -> Self {
        let mut characters = input.chars();
        let sign = characters.next().unwrap();
        let number = characters.collect::<String>().parse::<i32>().unwrap();
        match sign {
            '+' => FrequencyChange::Increase(number),
            '-' => FrequencyChange::Decrease(number),
            c => panic!("Unexpected character '{}'", c),
        }
    }

    pub fn apply(&self, value: i32) -> i32 {
        match self {
            FrequencyChange::Increase(n) => value + n,
            FrequencyChange::Decrease(n) => value - n,
        }
    }
}

fn read_frequency_changes_from_input(input: &Input) -> Vec<FrequencyChange> {
    input.value.lines().map(FrequencyChange::from_str).collect()
}

fn calculate_frequency(changes: Vec<FrequencyChange>) -> i32 {
    changes.into_iter().fold(0, |acc, change| change.apply(acc))
}

fn find_first_repeated_frequency(changes: Vec<FrequencyChange>) -> i32 {
    let mut already_seen = HashSet::<i32>::new();
    let mut pending_changes = VecDeque::from(changes.clone());

    let mut frequency = 0;
    already_seen.insert(frequency);

    loop {
        if pending_changes.is_empty() {
            pending_changes.clone_from(&VecDeque::from(changes.clone()));
        }

        let next_frequency = pending_changes.pop_front().unwrap().apply(frequency);

        if already_seen.contains(&next_frequency) {
            break next_frequency;
        }

        already_seen.insert(next_frequency);
        frequency = next_frequency;
    }
}

fn part_one(input: &Input) -> i32 {
    calculate_frequency(read_frequency_changes_from_input(input))
}

fn part_two(input: &Input) -> i32 {
    find_first_repeated_frequency(read_frequency_changes_from_input(input))
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

        Ok(assert_eq!(part_one(&input), 490))
    }

    #[test]
    fn test_part_two_solution() -> std::io::Result<()> {
        let input = Input::from_file("input.txt")?;

        Ok(assert_eq!(part_two(&input), 70357))
    }

    #[test]
    fn test_calculate_frequency() {
        let changes = vec![
            FrequencyChange::Increase(1),
            FrequencyChange::Increase(1),
            FrequencyChange::Increase(1),
        ];

        assert_eq!(calculate_frequency(changes), 3)
    }

    #[test]
    fn test_find_first_repeated_frequency() {
        let changes = vec![FrequencyChange::Increase(1), FrequencyChange::Decrease(1)];

        assert_eq!(find_first_repeated_frequency(changes), 0)
    }
}
