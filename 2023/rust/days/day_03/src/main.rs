use adventurous::Input;
use anyhow::Result;

#[adventurous::part_one]
fn part_one(input: &Input) -> Result<usize> {
    todo!()
}

#[adventurous::part_two]
fn part_two(input: &Input) -> Result<usize> {
    todo!()
}

fn main() -> Result<()> {
    adventurous::run("days/day_03/input.txt", part_one, part_two)
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    use super::*;

    adventurous::test_solutions!();
}
