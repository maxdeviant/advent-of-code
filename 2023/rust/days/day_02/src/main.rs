use anyhow::Result;

use day_02::{part_one, part_two};

fn main() -> Result<()> {
    adventurous::run("days/day_02/input.txt", part_one, part_two)
}
