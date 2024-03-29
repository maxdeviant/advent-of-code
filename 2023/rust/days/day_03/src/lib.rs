use std::collections::HashSet;
use std::iter::Sum;

use adventurous::Input;
use anyhow::Result;
use indexmap::IndexMap;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
enum Glyph {
    Dot,
    Digit(u8),
    Symbol(char),
}

impl Glyph {
    pub fn parse(char: char) -> Self {
        match char {
            '.' => Glyph::Dot,
            char if char.is_ascii_digit() => Glyph::Digit(char.to_digit(10).unwrap() as u8),
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
    pub const fn xy(x: usize, y: usize) -> Self {
        Self { x, y }
    }

    pub fn neighbors(&self) -> Neighbors {
        Neighbors {
            point: *self,
            index: 0,
        }
    }

    pub fn adjacent_to(&self, other: Point) -> bool {
        let dx = (self.x as isize - other.x as isize).abs();
        let dy = (self.y as isize - other.y as isize).abs();

        dx <= 1 && dy <= 1
    }
}

struct Neighbors {
    point: Point,
    index: usize,
}

impl Neighbors {
    const DIRECTIONS: [(isize, isize); 8] = [
        (0, 1),
        (1, 1),
        (1, 0),
        (1, -1),
        (0, -1),
        (-1, -1),
        (-1, 0),
        (-1, 1),
    ];
}

impl Iterator for Neighbors {
    type Item = Point;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= Self::DIRECTIONS.len() {
            return None;
        }

        let (dx, dy) = Self::DIRECTIONS[self.index];

        self.index += 1;

        let x = self.point.x as isize + dx;
        let y = self.point.y as isize + dy;

        if x < 0 || y < 0 {
            return None;
        }

        Some(Point::xy(x as usize, y as usize))
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
struct Span {
    pub lo: Point,
    pub hi: Point,
}

impl Span {
    pub const EMPTY: Self = Self {
        lo: Point::xy(usize::MAX, usize::MAX),
        hi: Point::xy(0, 0),
    };

    pub fn adjacent_to(&self, point: Point) -> bool {
        self.lo.adjacent_to(point) || self.hi.adjacent_to(point)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct PartNumber(usize);

impl PartNumber {
    pub fn from_digits(digits: &[u8]) -> Self {
        Self(
            digits
                .iter()
                .fold(0, |acc, digit| acc * 10 + *digit as usize),
        )
    }
}

impl Sum<PartNumber> for usize {
    fn sum<I: Iterator<Item = PartNumber>>(iter: I) -> Self {
        iter.fold(0, |acc, part_number| acc + part_number.0)
    }
}

struct Schematic {
    glyphs_by_coordinates: IndexMap<Point, Glyph>,
    symbols_by_coordinates: IndexMap<Point, char>,
}

impl Schematic {
    pub fn parse(input: &Input) -> Self {
        let glyphs_by_coordinates = input
            .lines()
            .enumerate()
            .flat_map(|(y, line)| {
                line.chars()
                    .map(Glyph::parse)
                    .enumerate()
                    .map(move |(x, glyph)| (Point::xy(x, y), glyph))
            })
            .collect::<IndexMap<_, _>>();

        let symbols_by_coordinates = glyphs_by_coordinates
            .iter()
            .filter_map(|(coords, glyph)| match glyph {
                Glyph::Symbol(symbol) => Some((*coords, *symbol)),
                _ => None,
            })
            .collect::<IndexMap<_, _>>();

        Self {
            glyphs_by_coordinates,
            symbols_by_coordinates,
        }
    }

    pub fn part_numbers(&self) -> impl Iterator<Item = PartNumber> {
        self.spanned_part_numbers()
            .into_iter()
            .map(|(part_number, _span)| part_number)
    }

    fn spanned_part_numbers(&self) -> Vec<(PartNumber, Span)> {
        let adjacent_to_symbols = self
            .symbols_by_coordinates
            .keys()
            .flat_map(|symbol_coords| symbol_coords.neighbors())
            .collect::<HashSet<_>>();

        let is_adjacent_to_symbol = |span: Span| {
            adjacent_to_symbols.contains(&span.lo) || adjacent_to_symbols.contains(&span.hi)
        };

        let mut spanned_numbers = Vec::new();
        let mut digits = Vec::with_capacity(3);
        let mut span = Span::EMPTY;

        for (coords, glyph) in &self.glyphs_by_coordinates {
            if let Glyph::Digit(digit) = glyph {
                digits.push(*digit);
                span.lo = span.lo.min(*coords);
                span.hi = span.hi.max(*coords);

                continue;
            }

            if digits.is_empty() {
                continue;
            }

            if is_adjacent_to_symbol(span) {
                let part_number = PartNumber::from_digits(&digits[..]);
                spanned_numbers.push((part_number, span));
            }

            digits.clear();
            span = Span::EMPTY;
        }

        spanned_numbers
    }

    pub fn gear_ratios(&self) -> Vec<usize> {
        let spanned_part_numbers = self.spanned_part_numbers();

        self.symbols_by_coordinates
            .iter()
            .filter_map(|(coords, char)| {
                if *char != '*' {
                    return None;
                }

                let mut adjacent_part_numbers = spanned_part_numbers
                    .iter()
                    .filter_map(|(part_number, span)| {
                        if span.adjacent_to(*coords) {
                            return Some(part_number);
                        }

                        None
                    })
                    .take(3);

                match (
                    adjacent_part_numbers.next(),
                    adjacent_part_numbers.next(),
                    adjacent_part_numbers.next(),
                ) {
                    (Some(first), Some(second), None) => Some(first.0 * second.0),
                    _ => None,
                }
            })
            .collect()
    }
}

#[adventurous::part_one(answer = "535078")]
pub fn part_one(input: &Input) -> Result<usize> {
    let schematic = Schematic::parse(input);

    Ok(schematic.part_numbers().sum())
}

#[adventurous::part_two(answer = "75312571")]
pub fn part_two(input: &Input) -> Result<usize> {
    let schematic = Schematic::parse(input);
    let gear_ratios = schematic.gear_ratios();

    Ok(gear_ratios.into_iter().sum())
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

    #[test]
    fn test_part_two_sample_input() -> Result<()> {
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

        assert_eq!(part_two(&Input::new(input.to_string()))?, 467835);

        Ok(())
    }
}
