use adventurous::Input;
use anyhow::{anyhow, Result};

fn parse_calibration_value(chars: impl IntoIterator<Item = char>) -> Result<i32> {
    let mut digits = chars.into_iter().filter(|char| char.is_digit(10));
    let first_digit = digits.next().ok_or(anyhow!("no first digit"))?;
    let second_digit = digits.last().unwrap_or(first_digit);

    Ok(format!("{first_digit}{second_digit}").parse::<i32>()?)
}

fn parse_digits(line: &str) -> Vec<char> {
    const REPLACEMENTS: [(&str, char); 9] = [
        ("one", '1'),
        ("two", '2'),
        ("three", '3'),
        ("four", '4'),
        ("five", '5'),
        ("six", '6'),
        ("seven", '7'),
        ("eight", '8'),
        ("nine", '9'),
    ];

    let mut digits = Vec::new();
    let mut current_word = Vec::new();

    for char in line.chars() {
        if char.is_digit(10) {
            digits.push(char);
            continue;
        }

        current_word.push(char);

        let word = current_word.iter().collect::<String>();

        for (pattern, replacement) in &REPLACEMENTS {
            if word.contains(pattern) {
                digits.push(*replacement);

                current_word = word.replace(pattern, "").chars().collect();
                break;
            }
        }
    }

    digits
}

fn part_one(input: &Input) -> Result<i32> {
    Ok(input
        .value
        .lines()
        .map(|line| parse_calibration_value(line.chars()))
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .sum())
}

fn part_two(input: &Input) -> Result<i32> {
    Ok(input
        .value
        .lines()
        .map(parse_digits)
        .map(parse_calibration_value)
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .sum())
}

fn main() -> Result<()> {
    let input = Input::from_file("days/day_01/input.txt")?;

    println!("Part One: {}", part_one(&input)?);
    println!("Part Two: {}", part_two(&input)?);

    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn test_part_one_solution() -> Result<()> {
        let input = Input::from_file("input.txt")?;

        Ok(assert_eq!(part_one(&input)?, 54667))
    }

    #[test]
    #[ignore = "not yet solved"]
    fn test_part_two_solution() -> Result<()> {
        let input = Input::from_file("input.txt")?;

        Ok(assert_eq!(part_two(&input)?, 0))
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

    #[test]
    fn test_part_two_sample_input() -> Result<()> {
        let input = indoc! {"
            two1nine
            eightwothree
            abcone2threexyz
            xtwone3four
            4nineeightseven2
            zoneight234
            7pqrstsixteen
        "};

        assert_eq!(part_two(&Input::new(input.to_string()))?, 281);

        Ok(())
    }

    #[test]
    fn test_parse_digits() {
        let cases = [
            ("two1nine", "219"),
            ("eightwothree", "83"),
            ("abcone2threexyz", "123"),
            ("xtwone3four", "234"),
            ("4nineeightseven2", "49872"),
            ("zoneight234", "1234"),
            ("7pqrstsixteen", "76"),
            ("sevenbsixsbzmone55", "76155"),
            ("twoxdmnvdnrd3smlzkbrqcvonekgzlbjgvnlpcvclcv2", "2312"),
            ("three2rtlcxqnbjj8fourhsevenbkmvpdone", "328471"),
        ];

        for (input, expected) in cases {
            let expected = expected.chars().collect::<Vec<_>>();

            assert_eq!(parse_digits(input), expected);
        }
    }
}
