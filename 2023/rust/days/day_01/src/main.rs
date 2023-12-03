use anyhow::Result;

use day_01::{part_one, part_two};

fn main() -> Result<()> {
    adventurous::run("days/day_01/input.txt", part_one, part_two)
}
