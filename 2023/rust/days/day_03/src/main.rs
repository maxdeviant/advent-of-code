use std::iter::Sum;

use adventurous::Input;
use anyhow::Result;
use indexmap::IndexMap;

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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct PartNumber(usize);

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
        let mut spanned_numbers = Vec::new();
        let mut current_number = Vec::new();

        for (coords, glyph) in &self.glyphs_by_coordinates {
            if let Glyph::Digit(digit) = glyph {
                current_number.push((coords, digit));
                continue;
            }

            if current_number.is_empty() {
                continue;
            }

            let (coords, digits): (Vec<Point>, Vec<&char>) =
                current_number.clone().into_iter().unzip();

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

        spanned_numbers
            .into_iter()
            .filter_map(move |(number, span)| {
                let is_adjacent_to_symbol = self
                    .symbols_by_coordinates
                    .keys()
                    .find(|symbol_coords| span.adjacent_to(**symbol_coords))
                    .is_some();

                Some((PartNumber(number), span)).filter(|_| is_adjacent_to_symbol)
            })
            .collect()
    }

    pub fn gear_ratios(&self) -> Vec<usize> {
        self.symbols_by_coordinates
            .iter()
            .filter_map(|(coords, char)| {
                if *char != '*' {
                    return None;
                }

                let adjacent_part_numbers = self
                    .spanned_part_numbers()
                    .into_iter()
                    .filter_map(|(part_number, span)| {
                        if span.adjacent_to(*coords) {
                            return Some(part_number);
                        }

                        None
                    })
                    .collect::<Vec<_>>();

                if adjacent_part_numbers.len() != 2 {
                    return None;
                }

                let part_number_one = adjacent_part_numbers[0];
                let part_number_two = adjacent_part_numbers[1];

                Some(part_number_one.0 * part_number_two.0)
            })
            .collect()
    }
}

#[adventurous::part_one(answer = "535078")]
fn part_one(input: &Input) -> Result<usize> {
    let schematic = Schematic::parse(&input);

    Ok(schematic.part_numbers().sum())
}

#[adventurous::part_two(answer = "75312571")]
fn part_two(input: &Input) -> Result<usize> {
    let schematic = Schematic::parse(&input);
    let gear_ratios = schematic.gear_ratios();

    Ok(gear_ratios.into_iter().sum())
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
