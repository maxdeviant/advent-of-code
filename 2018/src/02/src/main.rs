extern crate adventurous;

use std::collections::HashMap;

use adventurous::Input;

fn count_letters(id: &str) -> HashMap<char, i32> {
    let mut letter_counts = HashMap::new();

    for letter in id.chars() {
        if !letter_counts.contains_key(&letter) {
            letter_counts.insert(letter, 0);
        }

        *letter_counts.get_mut(&letter).unwrap() += 1;
    }

    letter_counts
}

fn contains_any_letter_two_times(id: &str) -> bool {
    count_letters(id)
        .into_iter()
        .any(|(_letter, occurences)| occurences == 2)
}

fn contains_any_letter_three_times(id: &str) -> bool {
    count_letters(id)
        .into_iter()
        .any(|(_letter, occurrences)| occurrences == 3)
}

fn part_one(input: &Input) -> usize {
    let twos = input
        .value
        .lines()
        .filter(|line| contains_any_letter_two_times(line))
        .count();
    let threes = input
        .value
        .lines()
        .filter(|line| contains_any_letter_three_times(line))
        .count();

    twos * threes
}

fn part_two(input: &Input) {}

fn main() -> std::io::Result<()> {
    let input = Input::from_file("input.txt")?;

    println!("Part One: {}", part_one(&input));

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one_solution() -> std::io::Result<()> {
        let input = Input::from_file("input.txt")?;

        Ok(assert_eq!(part_one(&input), 7350))
    }

    #[test]
    fn test_part_two_solution() -> std::io::Result<()> {
        let input = Input::from_file("input.txt")?;

        Ok(())
    }
}
