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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
struct Point {
    pub x: usize,
    pub y: usize,
}

impl Point {
    pub fn xy(x: usize, y: usize) -> Self {
        Self { x, y }
    }

    pub fn adjacent_to(&self, other: Point) -> bool {
        let dx = (self.x as isize - other.x as isize).abs();
        let dy = (self.y as isize - other.y as isize).abs();

        dx <= 1 && dy <= 1
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
struct Span {
    pub lo: Point,
    pub hi: Point,
}

impl Span {
    pub fn new(lo: Point, hi: Point) -> Self {
        Self { lo, hi }
    }

    pub fn adjacent_to(&self, point: Point) -> bool {
        self.lo.adjacent_to(point) || self.hi.adjacent_to(point)
    }
}

struct Schematic {
    glyphs_with_coordinates: Vec<(Point, Glyph)>,
}

impl Schematic {
    pub fn parse(input: &Input) -> Self {
        let glyphs_with_coordinates = input
            .lines()
            .enumerate()
            .flat_map(|(y, line)| {
                line.chars()
                    .map(Glyph::parse)
                    .enumerate()
                    .map(move |(x, glyph)| (Point::xy(x, y), glyph))
            })
            .collect();

        Self {
            glyphs_with_coordinates,
        }
    }
}

#[adventurous::part_one(answer = "535078")]
fn part_one(input: &Input) -> Result<usize> {
    let schematic = Schematic::parse(&input);

    let mut spanned_numbers = Vec::new();
    let mut current_number = Vec::new();

    for (coords, glyph) in &schematic.glyphs_with_coordinates {
        if let Glyph::Digit(digit) = glyph {
            current_number.push((coords, digit));
            continue;
        }

        if current_number.is_empty() {
            continue;
        }

        let (coords, digits): (Vec<Point>, Vec<&char>) = current_number.clone().into_iter().unzip();

        let part_number = digits
            .into_iter()
            .collect::<String>()
            .parse::<usize>()
            .unwrap();

        let lo = coords.iter().min().cloned().unwrap();
        let hi = coords.iter().max().cloned().unwrap();

        spanned_numbers.push((part_number, Span::new(lo, hi)));

        current_number.clear();
    }

    let symbol_coordinates = schematic
        .glyphs_with_coordinates
        .iter()
        .filter_map(|(coords, glyph)| match glyph {
            Glyph::Symbol(_) => Some(*coords),
            _ => None,
        })
        .collect::<HashSet<_>>();

    let part_numbers = spanned_numbers.into_iter().filter_map(|(number, span)| {
        let is_adjacent_to_symbol = symbol_coordinates
            .iter()
            .find(|symbol_coords| span.adjacent_to(**symbol_coords))
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
