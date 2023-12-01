use adventurous::Input;
use anyhow::{anyhow, Result};

fn part_one(input: &Input) -> Result<i32> {
    Ok(input
        .value
        .lines()
        .map(parse_calibration_value)
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .sum())
}

fn parse_calibration_value(line: &str) -> Result<i32> {
    let mut digits = line.chars().filter(|char| char.is_digit(10));
    let first_digit = digits.next().ok_or(anyhow!("no first digit"))?;
    let second_digit = digits.last().unwrap_or(first_digit);

    Ok(format!("{first_digit}{second_digit}").parse::<i32>()?)
}

fn part_two(input: &Input) -> i32 {
    todo!()
}

fn main() -> Result<()> {
    let input = Input::from_file("days/day_01/input.txt")?;

    println!("Part One: {}", part_one(&input)?);
    println!("Part Two: {}", part_two(&input));

    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    #[ignore = "not yet solved"]
    fn test_part_one_solution() -> Result<()> {
        let input = Input::from_file("input.txt")?;

        Ok(assert_eq!(part_one(&input)?, 0))
    }

    #[test]
    #[ignore = "not yet solved"]
    fn test_part_two_solution() -> Result<()> {
        let input = Input::from_file("input.txt")?;

        Ok(assert_eq!(part_two(&input), 0))
    }

    #[test]
    fn test_part_one_sample_input() -> Result<()> {
        let input = indoc! {"
            1abc2
            pqr3stu8vwx
            a1b2c3d4e5f
            treb7uchet
        "};

        assert_eq!(part_one(&Input::new(input.to_string()))?, 142);

        Ok(())
    }
}
