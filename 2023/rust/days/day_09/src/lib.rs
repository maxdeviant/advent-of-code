use adventurous::Input;
use anyhow::Result;

#[adventurous::part_one]
pub fn part_one(input: &Input) -> Result<usize> {
    todo!()
}

#[adventurous::part_two]
pub fn part_two(input: &Input) -> Result<usize> {
    todo!()
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    use super::*;

    adventurous::test_solutions!();

    #[test]
    #[ignore = "not yet solved"]
    fn test_part_one_sample_input() -> Result<()> {
        let input = indoc! {"

        "};

        assert_eq!(part_one(&Input::new(input.to_string()))?, 0);

        Ok(())
    }

    #[test]
    #[ignore = "not yet solved"]
    fn test_part_two_sample_input() -> Result<()> {
        let input = indoc! {"

        "};

        assert_eq!(part_two(&Input::new(input.to_string()))?, 0);

        Ok(())
    }
}
