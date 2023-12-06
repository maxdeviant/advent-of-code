mod parser;

use adventurous::Input;
use anyhow::{anyhow, Result};
use indexmap::IndexMap;
use nom::Finish;

use crate::parser::parse_almanac;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
struct Seed(usize);

#[derive(Debug)]
struct MapRange {
    pub destination_start: usize,
    pub source_start: usize,
    pub length: usize,
}

impl MapRange {
    pub fn translate(&self, n: usize) -> usize {
        let position_in_range = n - self.source_start;
        let destination = self.destination_start + position_in_range;

        destination
    }
}

#[derive(Debug)]
struct Map {
    pub name: String,
    pub ranges: Vec<MapRange>,
}

#[derive(Debug)]
struct Almanac {
    seeds: Vec<Seed>,
    maps: IndexMap<String, Map>,
}

impl Almanac {
    fn parse(input: &Input) -> Result<Self> {
        let (_, game) = parse_almanac(input.raw())
            .finish()
            .map_err(|err| nom::error::Error {
                code: err.code,
                input: err.input.to_string(),
            })?;
        Ok(game)
    }
}

#[adventurous::part_one(answer = "318728750")]
pub fn part_one(input: &Input) -> Result<usize> {
    let almanac = Almanac::parse(input)?;

    let chain = [
        "seed-to-soil map",
        "soil-to-fertilizer map",
        "fertilizer-to-water map",
        "water-to-light map",
        "light-to-temperature map",
        "temperature-to-humidity map",
        "humidity-to-location map",
    ];

    let mut locations = Vec::new();

    for seed in almanac.seeds {
        let mut number = seed.0;

        for map_name in chain {
            let map = almanac.maps.get(map_name).unwrap();

            if let Some(range) = map.ranges.iter().find(|range| {
                (range.source_start..range.source_start + range.length).contains(&number)
            }) {
                number = range.translate(number);
            }
        }

        locations.push(number);
    }

    locations
        .into_iter()
        .min()
        .ok_or_else(|| anyhow!("no lowest location number"))
}

#[adventurous::part_two]
pub fn part_two(input: &Input) -> Result<usize> {
    todo!()
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
            seeds: 79 14 55 13

            seed-to-soil map:
            50 98 2
            52 50 48

            soil-to-fertilizer map:
            0 15 37
            37 52 2
            39 0 15

            fertilizer-to-water map:
            49 53 8
            0 11 42
            42 0 7
            57 7 4

            water-to-light map:
            88 18 7
            18 25 70

            light-to-temperature map:
            45 77 23
            81 45 19
            68 64 13

            temperature-to-humidity map:
            0 69 1
            1 0 69

            humidity-to-location map:
            60 56 37
            56 93 4
        "};

        assert_eq!(part_one(&Input::new(input.to_string()))?, 35);

        Ok(())
    }
}
