use anyhow::Result;

use day_03::{part_one, part_two};

fn main() -> Result<()> {
    adventurous::run("days/day_03/input.txt", part_one, part_two)
}
