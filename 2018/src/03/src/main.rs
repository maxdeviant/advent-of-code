extern crate adventurous;

use adventurous::Input;

#[derive(Debug)]
struct Claim {
    pub id: String,
    pub x: i32,
    pub y: i32,
    pub width: i32,
    pub height: i32,
}

impl Claim {
    pub fn from_str(value: &str) -> Self {
        let mut parts = value.split_whitespace();
        let id_part = parts.next();
        let _ = parts.next();
        let coordinates_part = parts.next();
        let dimensions_part = parts.next();

        let (x, y) = {
            let coordinates = coordinates_part
                .map(|x| {
                    x[..x.len() - 1]
                        .split(',')
                        .map(|x| x.parse::<i32>().unwrap())
                        .collect()
                })
                .unwrap_or(vec![]);
            match coordinates.as_slice() {
                &[x, y] => (x, y),
                _ => unreachable!(),
            }
        };

        let (width, height) = {
            let dimensions = dimensions_part
                .map(|x| x.split('x').map(|x| x.parse::<i32>().unwrap()).collect())
                .unwrap_or(vec![]);
            match dimensions.as_slice() {
                &[width, height] => (width, height),
                _ => unreachable!(),
            }
        };

        Self {
            id: id_part.unwrap_or("").to_string(),
            x,
            y,
            width,
            height,
        }
    }
}

fn part_one(input: &Input) -> i32 {
    let claims = input.value.lines().map(Claim::from_str);

    for claim in claims {
        println!("{:?}", claim);
    }

    0
}

fn main() -> std::io::Result<()> {
    let input = Input::from_file("input.txt")?;

    println!("Part One: {}", part_one(&input));

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one_solution() -> std::io::Result<()> {
        let input = Input::from_file("input.txt")?;

        Ok(assert_eq!(true, true))
    }

    #[test]
    fn test_part_two_solution() -> std::io::Result<()> {
        let input = Input::from_file("input.txt")?;

        Ok(assert_eq!(true, true))
    }
}
