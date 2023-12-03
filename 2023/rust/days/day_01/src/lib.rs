use std::collections::VecDeque;

use adventurous::Input;
use anyhow::{anyhow, Result};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
enum ParsingStrategy {
    DigitsOnly,
    DigitsAndWords,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
enum ParseDirection {
    Forwards,
    Backwards,
}

struct DigitLocator {
    strategy: ParsingStrategy,
}

impl DigitLocator {
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

    fn digits_only() -> Self {
        Self {
            strategy: ParsingStrategy::DigitsOnly,
        }
    }

    fn digits_and_words() -> Self {
        Self {
            strategy: ParsingStrategy::DigitsAndWords,
        }
    }

    fn digit(&self, direction: ParseDirection, input: &str) -> Option<char> {
        let mut current_word = VecDeque::new();

        let chars: Box<dyn Iterator<Item = _>> = match direction {
            ParseDirection::Forwards => Box::new(input.chars()),
            ParseDirection::Backwards => Box::new(input.chars().rev()),
        };

        for char in chars {
            if char.is_ascii_digit() {
                return Some(char);
            }

            match self.strategy {
                ParsingStrategy::DigitsOnly => {}
                ParsingStrategy::DigitsAndWords => {
                    match direction {
                        ParseDirection::Forwards => current_word.push_back(char),
                        ParseDirection::Backwards => current_word.push_front(char),
                    }

                    let word = current_word.iter().collect::<String>();

                    for (pattern, digit) in &Self::REPLACEMENTS {
                        if word.contains(pattern) {
                            return Some(*digit);
                        }
                    }
                }
            }
        }

        None
    }

    fn first(&self, input: &str) -> Option<char> {
        self.digit(ParseDirection::Forwards, input)
    }

    fn last(&self, input: &str) -> Option<char> {
        self.digit(ParseDirection::Backwards, input)
    }
}

fn parse_calibration_value(digit_locator: &DigitLocator, line: &str) -> Result<usize> {
    let first_digit = digit_locator.first(line).ok_or(anyhow!("no first digit"))?;
    let last_digit = digit_locator.last(line).ok_or(anyhow!("no last digit"))?;

    Ok(format!("{first_digit}{last_digit}").parse::<usize>()?)
}

#[adventurous::part_one(answer = "54667")]
pub fn part_one(input: &Input) -> Result<usize> {
    let digit_locator = DigitLocator::digits_only();

    let calibration_values =
        input.traverse(|line| parse_calibration_value(&digit_locator, line))?;

    Ok(calibration_values.sum())
}

#[adventurous::part_two(answer = "54203")]
pub fn part_two(input: &Input) -> Result<usize> {
    let digit_locator = DigitLocator::digits_and_words();

    let calibration_values =
        input.traverse(|line| parse_calibration_value(&digit_locator, line))?;

    Ok(calibration_values.sum())
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
    fn test_digit_locator() {
        let cases = [
            ("oneight", '1', '8'),
            ("eighthree", '8', '3'),
            ("two1nine", '2', '9'),
            ("eightwothree", '8', '3'),
            ("abcone2threexyz", '1', '3'),
            ("xtwone3four", '2', '4'),
            ("4nineeightseven2", '4', '2'),
            ("zoneight234", '1', '4'),
            ("7pqrstsixteen", '7', '6'),
            ("sevenbsixsbzmone55", '7', '5'),
            ("twoxdmnvdnrd3smlzkbrqcvonekgzlbjgvnlpcvclcv2", '2', '2'),
            ("three2rtlcxqnbjj8fourhsevenbkmvpdone", '3', '1'),
            ("9zngoneoneightzdz", '9', '8'),
        ];

        let locator = DigitLocator::digits_and_words();

        for (input, expected_first, expected_last) in cases {
            assert_eq!(
                locator.first(input),
                Some(expected_first),
                "no first digit found for '{input}'"
            );
            assert_eq!(
                locator.last(input),
                Some(expected_last),
                "no last digit found for '{input}'"
            );
        }
    }
}
