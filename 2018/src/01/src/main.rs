use std::fs::File;
use std::io::Read;

enum FrequencyChange {
    Increase(i32),
    Decrease(i32),
}

fn calculate_frequency(changes: Vec<FrequencyChange>) -> i32 {
    changes.into_iter().fold(0, |acc, change| match change {
        FrequencyChange::Increase(n) => acc + n,
        FrequencyChange::Decrease(n) => acc - n,
    })
}

fn part_one(input: String) -> i32 {
    calculate_frequency(
        input
            .lines()
            .map(|line| {
                let mut characters = line.chars();
                let sign = characters.next().unwrap();
                let number = characters.collect::<String>().parse::<i32>().unwrap();
                match sign {
                    '+' => FrequencyChange::Increase(number),
                    '-' => FrequencyChange::Decrease(number),
                    c => panic!("Unexpected character '{}'", c),
                }
            })
            .collect(),
    )
}

fn main() -> std::io::Result<()> {
    let mut file = File::open("input.txt")?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    println!("Part One: {}", part_one(contents));

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_calculate_frequency() {
        let changes = vec![
            FrequencyChange::Increase(1),
            FrequencyChange::Increase(1),
            FrequencyChange::Increase(1),
        ];

        assert_eq!(calculate_frequency(changes), 3)
    }
}
