use adventurous::Input;
use anyhow::Result;

use day_09::{part_one, part_two};

fn main() {
    divan::main();
}

#[divan::bench(name = "part_one")]
fn part_one_bench() -> Result<i32> {
    part_one(divan::black_box(&Input::from(include_str!("../input.txt"))))
}

#[divan::bench(name = "part_two")]
fn part_two_bench() -> Result<i32> {
    part_two(divan::black_box(&Input::from(include_str!("../input.txt"))))
}
