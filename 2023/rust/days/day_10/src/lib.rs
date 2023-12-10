use std::collections::{HashMap, VecDeque};

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

    pub fn neighbors<'map>(&self, map: &'map Map) -> Vec<&'map Tile> {
        use Direction::*;

        [North, South, East, West]
            .into_iter()
            .flat_map(|direction| {
                self.neighbor(map, direction)
                    .filter(|tile| tile.pipe.is_some())
            })
            .collect()
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

impl Map {
    pub fn parse(input: &Input) -> Self {
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

        Self { tiles }
    }

    pub fn compute_distances(&self) -> HashMap<(i32, i32), usize> {
        let start_tile = self
            .tiles
            .values()
            .find(|tile| tile.is_starting_position)
            .unwrap();

        let mut distances_by_tile = HashMap::new();
        let mut candidates = VecDeque::new();

        candidates.push_back(start_tile);

        while let Some(tile) = candidates.pop_front() {
            if tile.is_starting_position {
                let neighbors = tile.neighbors(&self);
                for neighbor in neighbors {
                    if neighbor.pipe.is_some() {
                        candidates.push_back(neighbor);
                    }
                }

                distances_by_tile.insert(tile.location, 0);

                continue;
            }

            let (tile_a, tile_b) = tile.connected_tiles(&self);

            let tile_a_distance = tile_a.and_then(|tile| distances_by_tile.get(&tile.location));
            let tile_b_distance = tile_b.and_then(|tile| distances_by_tile.get(&tile.location));

            if let Some(distance) = tile_a_distance.or(tile_b_distance) {
                if let Some(tile_a) = tile_a {
                    if !distances_by_tile.contains_key(&tile_a.location) {
                        candidates.push_back(tile_a);
                    }
                }

                if let Some(tile_b) = tile_b {
                    if !distances_by_tile.contains_key(&tile_b.location) {
                        candidates.push_back(tile_b);
                    }
                }

                distances_by_tile.insert(tile.location, distance + 1);
            }
        }

        distances_by_tile
    }

    #[cfg(test)]
    pub fn print(&self, distances_by_tile: &HashMap<(i32, i32), usize>) {
        let (width, height) = self.tiles.keys().max().copied().unwrap();

        for y in 0..=height {
            for x in 0..=width {
                if let Some(distance) = distances_by_tile.get(&(x, y)) {
                    print!("{distance}");
                } else {
                    print!(".");
                }
            }
            println!("");
        }
    }
}

#[adventurous::part_one(answer = "6951")]
pub fn part_one(input: &Input) -> Result<usize> {
    let map = Map::parse(input);
    let distances_by_tile = map.compute_distances();

    #[cfg(test)]
    map.print(&distances_by_tile);

    distances_by_tile
        .values()
        .max()
        .copied()
        .ok_or_else(|| anyhow!("no max distance"))
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
