use std::num::ParseIntError;

use adventurous::Input;

fn calculate_carried_calories(input: &Input) -> Result<Vec<i32>, ParseIntError> {
    let chunked_lines = input
        .value
        .lines()
        .into_iter()
        .fold(Vec::new(), |mut acc, line| {
            if line.is_empty() {
                acc.push(Vec::new());
                return acc;
            }

            if let Some(last_chunk) = acc.last_mut() {
                last_chunk.push(line);
                return acc;
            }

            return acc;
        });

    chunked_lines
        .into_iter()
        .map(|chunk| {
            chunk
                .iter()
                .map(|line| line.parse::<i32>())
                .collect::<Result<Vec<_>, _>>()
                .map(|calories| calories.iter().sum())
        })
        .collect::<Result<Vec<i32>, _>>()
}

fn part_one(input: &Input) -> i32 {
    calculate_carried_calories(&input)
        .expect("failed to calculate carried calories")
        .into_iter()
        .max()
        .expect("no elf carrying the most calories")
}

fn part_two(input: &Input) -> i32 {
    let mut carried_calories =
        calculate_carried_calories(&input).expect("failed to calculate carried calories");

    carried_calories.sort_unstable();
    carried_calories.reverse();

    carried_calories.into_iter().take(3).sum()
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

        Ok(assert_eq!(part_one(&input), 71934))
    }

    #[test]
    fn test_part_two_solution() -> std::io::Result<()> {
        let input = Input::from_file("input.txt")?;

        Ok(assert_eq!(part_two(&input), 211447))
    }
}
