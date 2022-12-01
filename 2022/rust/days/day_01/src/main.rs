use adventurous::Input;

fn part_one(input: &Input) -> i32 {
    let chunked_lines = input
        .value
        .lines()
        .into_iter()
        .fold(Vec::new(), |mut acc, line| {
            if line.is_empty() {
                acc.push(Vec::new());
                return acc;
            }

            if let Some(last_chunk) = acc.last_mut() {
                last_chunk.push(line);
                return acc;
            }

            return acc;
        });

    let elves = chunked_lines
        .into_iter()
        .map(|chunk| {
            chunk
                .iter()
                .map(|line| line.parse::<i32>())
                .collect::<Result<Vec<_>, _>>()
                .map(|calories| calories.iter().sum())
        })
        .collect::<Result<Vec<i32>, _>>()
        .expect("failed to parse input");

    elves
        .into_iter()
        .max()
        .expect("no elf carrying the most calories")
}

fn part_two(input: &Input) -> i32 {
    0
}

fn main() -> std::io::Result<()> {
    let input = Input::from_file("input.txt")?;

    println!("Part One: {}", part_one(&input));
    println!("Part Two: {}", part_two(&input));

    Ok(())
}
