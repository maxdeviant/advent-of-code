use adventurous::Input;
use anyhow::Result;

fn part_one(input: &Input) -> i32 {
    todo!()
}

fn part_two(input: &Input) -> i32 {
    todo!()
}

fn main() -> Result<()> {
    let input = Input::from_file("input.txt")?;

    println!("Part One: {}", part_one(&input));
    println!("Part Two: {}", part_two(&input));

    Ok(())
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    #[ignore = "not yet solved"]
    fn test_part_one_solution() -> Result<()> {
        let input = Input::from_file("input.txt")?;

        Ok(assert_eq!(part_one(&input), 0))
    }

    #[test]
    #[ignore = "not yet solved"]
    fn test_part_two_solution() -> Result<()> {
        let input = Input::from_file("input.txt")?;

        Ok(assert_eq!(part_two(&input), 0))
    }
}
