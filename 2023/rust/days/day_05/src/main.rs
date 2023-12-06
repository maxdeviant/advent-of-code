use anyhow::Result;

use day_05::{part_one, part_two};

#[cfg(feature = "dhat-heap")]
#[global_allocator]
static ALLOC: dhat::Alloc = dhat::Alloc;

fn main() -> Result<()> {
    #[cfg(feature = "dhat-heap")]
    let _profiler = dhat::Profiler::new_heap();

    adventurous::run("days/day_05/input.txt", part_one, part_two)
}
