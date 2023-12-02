mod parser;

use adventurous::Input;
use anyhow::Result;
use enum_map::{enum_map, Enum, EnumMap};
use nom::Finish;

use crate::parser::parse_game;
use crate::Cube::*;

#[derive(Debug, Enum, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
enum Cube {
    Red,
    Green,
    Blue,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
enum GamePossibility {
    Possible,
    Impossible,
}

#[derive(Debug, PartialEq)]
pub struct Game {
    id: usize,
    sets: Vec<Vec<(Cube, usize)>>,
}

impl Game {
    fn parse(input: &str) -> Result<Self> {
        let (_, game) = parse_game(input)
            .finish()
            .map_err(|err| nom::error::Error {
                code: err.code,
                input: err.input.to_string(),
            })?;
        Ok(game)
    }

    fn possibility(&self, cube_counts: &EnumMap<Cube, usize>) -> GamePossibility {
        for set in &self.sets {
            if set
                .iter()
                .find(|(cube, count)| *count > cube_counts[*cube])
                .is_some()
            {
                return GamePossibility::Impossible;
            }
        }

        GamePossibility::Possible
    }

    /// Returns the minimum number of cubes required for this game to be possible
    fn minimum_cubes_required(&self) -> EnumMap<Cube, usize> {
        let mut minimum_cubes_required = enum_map! {
            Red => 0,
            Green => 0,
            Blue => 0,
        };

        for set in &self.sets {
            for (cube, count) in set {
                minimum_cubes_required[*cube] = minimum_cubes_required[*cube].max(*count);
            }
        }

        minimum_cubes_required
    }

    fn minimum_cubes_power(&self) -> usize {
        let minimum_cubes = self.minimum_cubes_required();

        minimum_cubes[Red] * minimum_cubes[Green] * minimum_cubes[Blue]
    }
}

fn part_one(input: &Input) -> Result<usize> {
    let cube_counts = enum_map! {
        Red => 12,
        Green => 13,
        Blue => 14,
    };

    let games = input
        .value
        .lines()
        .map(Game::parse)
        .collect::<Result<Vec<_>, _>>()?;

    Ok(games
        .into_iter()
        .filter(|game| game.possibility(&cube_counts) == GamePossibility::Possible)
        .map(|game| game.id)
        .sum())
}

fn part_two(input: &Input) -> Result<usize> {
    let games = input
        .value
        .lines()
        .map(Game::parse)
        .collect::<Result<Vec<_>, _>>()?;

    Ok(games
        .into_iter()
        .map(|game| game.minimum_cubes_power())
        .sum())
}

fn main() -> Result<()> {
    let input = Input::from_file("days/day_02/input.txt")?;

    println!("Part One: {}", part_one(&input)?);
    println!("Part Two: {}", part_two(&input)?);

    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn test_part_one_solution() -> Result<()> {
        let input = Input::from_file("input.txt")?;

        Ok(assert_eq!(part_one(&input)?, 1853))
    }

    #[test]
    fn test_part_two_solution() -> Result<()> {
        let input = Input::from_file("input.txt")?;

        Ok(assert_eq!(part_two(&input)?, 72706))
    }

    #[test]
    fn test_part_one_sample_input() -> Result<()> {
        let input = indoc! {"
            Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
            Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
            Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
            Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
            Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
        "};

        assert_eq!(part_one(&Input::new(input.to_string()))?, 8);

        Ok(())
    }

    #[test]
    fn test_part_two_sample_input() -> Result<()> {
        let input = indoc! {"
            Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
            Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
            Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
            Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
            Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
        "};

        assert_eq!(part_two(&Input::new(input.to_string()))?, 2286);

        Ok(())
    }

    #[test]
    fn test_parse_game() {
        let game = Game::parse("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green").unwrap();

        assert_eq!(
            game,
            Game {
                id: 1,
                sets: vec![
                    vec![(Blue, 3), (Red, 4)],
                    vec![(Red, 1), (Green, 2), (Blue, 6)],
                    vec![(Green, 2)],
                ]
            }
        )
    }

    #[test]
    fn test_minimum_cubes_required() {
        let game = Game::parse("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green").unwrap();

        assert_eq!(
            game.minimum_cubes_required(),
            enum_map! {
                Red => 4,
                Green => 2,
                Blue => 6,
            }
        )
    }

    #[test]
    fn test_minimum_cubes_power() {
        let game =
            Game::parse("Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red")
                .unwrap();

        assert_eq!(game.minimum_cubes_power(), 1560);
    }
}
