extern crate adventurous;

use adventurous::{CharCounts, Input};

fn contains_any_letter_two_times(id: &str) -> bool {
    id.char_counts()
        .into_iter()
        .any(|(_letter, occurences)| occurences == 2)
}

fn contains_any_letter_three_times(id: &str) -> bool {
    id.char_counts()
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

fn diff_ids(a: &str, b: &str) -> Vec<usize> {
    let mut differences = Vec::new();

    for (index, character) in a.char_indices() {
        for (index_b, character_b) in b.char_indices() {
            if index == index_b && character != character_b {
                differences.push(index);
            }
        }
    }

    differences
}

fn part_two(input: &Input) -> Option<String> {
    for line in input.value.lines() {
        for line_b in input.value.lines() {
            let diff = diff_ids(line, line_b);
            if diff.len() == 1 {
                let differing_index = diff.first().unwrap();
                let mut characters: Vec<char> = line.chars().collect();
                characters.remove(*differing_index);
                return Some(characters.iter().collect());
            }
        }
    }

    None
}

fn main() -> std::io::Result<()> {
    let input = Input::from_file("input.txt")?;

    println!("Part One: {}", part_one(&input));
    println!("Part Two: {}", part_two(&input).unwrap());

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

        Ok(assert_eq!(
            part_two(&input),
            Some(String::from("wmlnjevbfodamyiqpucrhsukg"))
        ))
    }

    #[test]
    fn test_diff_ids() {
        assert_eq!(diff_ids("fghij", "fguij"), vec![2])
    }
}
