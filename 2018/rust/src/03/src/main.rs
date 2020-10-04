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
            id: id_part.unwrap_or("")[1..].to_string(),
            x,
            y,
            width,
            height,
        }
    }
}

fn count_overlapping_squares(claims: Vec<Claim>) -> i32 {
    let mut fabric: Vec<Vec<i32>> = Vec::with_capacity(1000);
    for x in 0..1000 {
        fabric.push(Vec::with_capacity(1000));
        for _y in 0..1000 {
            fabric[x].push(0);
        }
    }

    for claim in claims {
        for x in claim.x..claim.x + claim.width {
            for y in claim.y..claim.y + claim.height {
                fabric[x as usize][y as usize] += 1;
            }
        }
    }

    let mut overlapping = 0;
    for x in 0..1000 {
        for y in 0..1000 {
            if fabric[x][y] > 1 {
                overlapping += 1;
            }
        }
    }

    overlapping
}

fn find_claim_with_no_overlaps(claims: Vec<Claim>) -> Option<Claim> {
    let mut fabric: Vec<Vec<i32>> = Vec::with_capacity(1000);
    for x in 0..1000 {
        fabric.push(Vec::with_capacity(1000));
        for _y in 0..1000 {
            fabric[x].push(0);
        }
    }

    for claim in &claims {
        for x in claim.x..claim.x + claim.width {
            for y in claim.y..claim.y + claim.height {
                fabric[x as usize][y as usize] += 1;
            }
        }
    }

    for claim in claims {
        let mut overlaps = false;

        for x in claim.x..claim.x + claim.width {
            for y in claim.y..claim.y + claim.height {
                if fabric[x as usize][y as usize] > 1 {
                    overlaps = true;
                }
            }
        }

        if !overlaps {
            return Some(claim);
        }
    }

    None
}

fn part_one(input: &Input) -> i32 {
    let claims = input.value.lines().map(Claim::from_str).collect();
    count_overlapping_squares(claims)
}

fn part_two(input: &Input) -> String {
    let claims = input.value.lines().map(Claim::from_str).collect();
    find_claim_with_no_overlaps(claims).unwrap().id
}

fn main() -> std::io::Result<()> {
    let input = Input::from_file("input.txt")?;

    println!("Part One: {}", part_one(&input));
    println!("Part Two: {}", part_two(&input));

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one_solution() -> std::io::Result<()> {
        let input = Input::from_file("input.txt")?;

        Ok(assert_eq!(part_one(&input), 97218))
    }

    #[test]
    fn test_part_two_solution() -> std::io::Result<()> {
        let input = Input::from_file("input.txt")?;

        Ok(assert_eq!(part_two(&input), "717"))
    }

    #[test]
    fn test_count_overlapping_squares() {
        let claims = vec![
            Claim::from_str("#1 @ 1,3: 4x4"),
            Claim::from_str("#2 @ 3,1: 4x4"),
            Claim::from_str("#3 @ 5,5: 2x2"),
        ];
        assert_eq!(count_overlapping_squares(claims), 4)
    }
}
