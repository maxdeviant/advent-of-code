use std::collections::HashMap;

use adventurous::Input;
use anyhow::{anyhow, Result};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub enum Direction {
    North,
    South,
    East,
    West,
}

impl Direction {
    pub fn delta(&self) -> (i32, i32) {
        match self {
            Self::North => (0, -1),
            Self::South => (0, 1),
            Self::East => (1, 0),
            Self::West => (-1, 0),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub enum Pipe {
    Vertical,
    Horizontal,
    NorthToEast,
    NorthToWest,
    SouthToWest,
    SouthToEast,
}

impl Pipe {
    pub fn parse(char: char) -> Result<Pipe> {
        match char {
            '|' => Ok(Self::Vertical),
            '-' => Ok(Self::Horizontal),
            'L' => Ok(Self::NorthToEast),
            'J' => Ok(Self::NorthToWest),
            '7' => Ok(Self::SouthToWest),
            'F' => Ok(Self::SouthToEast),
            _ => Err(anyhow!("Invalid pipe: '{char}'")),
        }
    }
}

#[derive(Debug)]
struct Tile {
    pub location: (i32, i32),
    pub pipe: Option<Pipe>,
    pub is_starting_position: bool,
}

impl Tile {
    pub fn neighbor<'map>(&self, map: &'map Map, direction: Direction) -> Option<&'map Tile> {
        let (x, y) = self.location;
        let (dx, dy) = direction.delta();

        let new_x = x + dx;
        let new_y = y + dy;

        map.tiles.get(&(new_x, new_y))
    }

    pub fn connected_tiles<'map>(
        &self,
        map: &'map Map,
    ) -> (Option<&'map Tile>, Option<&'map Tile>) {
        if self.is_starting_position {
            let mut neighbors = [North, South, East, West]
                .into_iter()
                .flat_map(|direction| {
                    self.neighbor(map, direction)
                        .filter(|tile| tile.pipe.is_some())
                });

            return (neighbors.next(), neighbors.next());
        }

        let Some(pipe) = self.pipe else {
            return (None, None);
        };

        use Direction::*;

        let (direction_one, direction_two) = match pipe {
            Pipe::Vertical => (North, South),
            Pipe::Horizontal => (East, West),
            Pipe::NorthToEast => (North, East),
            Pipe::NorthToWest => (North, West),
            Pipe::SouthToWest => (South, West),
            Pipe::SouthToEast => (South, East),
        };

        let tile_one = self.neighbor(map, direction_one);
        let tile_two = self.neighbor(map, direction_two);

        (tile_one, tile_two)
    }
}

struct Map {
    pub tiles: HashMap<(i32, i32), Tile>,
}

#[adventurous::part_one]
pub fn part_one(input: &Input) -> Result<usize> {
    let tiles = input
        .lines()
        .enumerate()
        .flat_map(|(y, line)| {
            line.chars().enumerate().map(move |(x, char)| Tile {
                location: (x as i32, y as i32),
                pipe: Pipe::parse(char).ok(),
                is_starting_position: char == 'S',
            })
        })
        .fold(HashMap::new(), |mut map, tile| {
            map.insert(tile.location, tile);
            map
        });

    let map = Map { tiles };

    let start_tile = map
        .tiles
        .values()
        .find(|tile| tile.is_starting_position)
        .unwrap();

    let mut distances_by_tile = HashMap::new();
    let mut current_location = start_tile.location;
    let mut steps = 0;

    loop {
        let current_tile = map.tiles.get(&current_location).unwrap();

        steps += 1;

        let (tile_a, tile_b) = current_tile.connected_tiles(&map);

        if let Some(tile_a) = tile_a {
            distances_by_tile.insert(tile_a.location, steps);
        }

        if let Some(tile_b) = tile_b {
            distances_by_tile.insert(tile_b.location, steps);
        }

        let next_location = tile_a
            .map(|tile_a| tile_a.location)
            .or(tile_b.map(|tile_b| tile_b.location))
            .expect("no next location");

        current_location = next_location;

        if current_location == start_tile.location {
            break;
        }
    }

    dbg!(distances_by_tile);

    todo!()
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
    #[ignore = "not yet solved"]
    fn test_part_one_sample_input() -> Result<()> {
        let input = indoc! {"
            .....
            .S-7.
            .|.|.
            .L-J.
            .....
        "};

        assert_eq!(part_one(&Input::new(input.to_string()))?, 4);

        Ok(())
    }

    #[test]
    #[ignore = "not yet solved"]
    fn test_part_two_sample_input() -> Result<()> {
        let input = indoc! {"

        "};

        assert_eq!(part_two(&Input::new(input.to_string()))?, 0);

        Ok(())
    }
}
