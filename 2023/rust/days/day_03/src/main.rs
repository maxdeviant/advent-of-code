use std::collections::HashSet;

use adventurous::Input;
use anyhow::Result;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
enum Glyph {
    Dot,
    Digit(char),
    Symbol(char),
}

impl Glyph {
    pub fn parse(char: char) -> Self {
        match char {
            '.' => Glyph::Dot,
            char if char.is_digit(10) => Glyph::Digit(char),
            char => Glyph::Symbol(char),
        }
    }
}

fn parse_glyphs_with_coordinates(input: &Input) -> Vec<((usize, usize), Glyph)> {
    input
        .lines()
        .enumerate()
        .flat_map(|(y, line)| {
            line.chars()
                .map(Glyph::parse)
                .enumerate()
                .map(move |(x, glyph)| ((x, y), glyph))
        })
        .collect()
}

fn are_adjacent((x1, y1): (usize, usize), (x2, y2): (usize, usize)) -> bool {
    let dx = (x1 as isize - x2 as isize).abs();
    let dy = (y1 as isize - y2 as isize).abs();

    dx <= 1 && dy <= 1
}

#[adventurous::part_one(answer = "535078")]
fn part_one(input: &Input) -> Result<usize> {
    let glyphs_with_coordinates = parse_glyphs_with_coordinates(&input);

    let mut spanned_numbers = Vec::new();
    let mut current_number = Vec::new();

    for ((x, y), glyph) in &glyphs_with_coordinates {
        if let Glyph::Digit(digit) = glyph {
            current_number.push(((x, y), digit));
            continue;
        }

        if current_number.is_empty() {
            continue;
        }

        let (coords, digits): (Vec<_>, Vec<&char>) = current_number.clone().into_iter().unzip();

        let part_number = digits
            .into_iter()
            .collect::<String>()
            .parse::<usize>()
            .unwrap();

        let lo = coords.iter().min().cloned().map(|(x, y)| (*x, *y)).unwrap();
        let hi = coords.iter().max().cloned().map(|(x, y)| (*x, *y)).unwrap();

        spanned_numbers.push((part_number, (lo, hi)));

        current_number.clear();
    }

    let symbol_coordinates = glyphs_with_coordinates
        .iter()
        .filter_map(|(coords, glyph)| match glyph {
            Glyph::Symbol(_) => Some(*coords),
            _ => None,
        })
        .collect::<HashSet<_>>();

    let part_numbers = spanned_numbers
        .into_iter()
        .filter_map(|(number, (lo, hi))| {
            let is_adjacent_to_symbol = symbol_coordinates
                .iter()
                .find(|symbol_coords| {
                    are_adjacent(lo, **symbol_coords) || are_adjacent(hi, **symbol_coords)
                })
                .is_some();

            Some(number).filter(|_| is_adjacent_to_symbol)
        });

    Ok(part_numbers.sum())
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

    #[test]
    fn test_part_one_sample_input() -> Result<()> {
        let input = indoc! {"
            467..114..
            ...*......
            ..35..633.
            ......#...
            617*......
            .....+.58.
            ..592.....
            ......755.
            ...$.*....
            .664.598..
        "};

        assert_eq!(part_one(&Input::new(input.to_string()))?, 4361);

        Ok(())
    }
}
