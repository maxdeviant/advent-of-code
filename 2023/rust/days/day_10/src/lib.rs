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

use Direction::*;

impl Direction {
    pub fn delta(&self) -> (i32, i32) {
        match self {
            North => (0, -1),
            South => (0, 1),
            East => (1, 0),
            West => (-1, 0),
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

    pub fn ends(&self) -> (Direction, Direction) {
        match self {
            Self::Vertical => (North, South),
            Self::Horizontal => (East, West),
            Self::NorthToEast => (North, East),
            Self::NorthToWest => (North, West),
            Self::SouthToWest => (South, West),
            Self::SouthToEast => (South, East),
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
        [North, South, East, West]
            .into_iter()
            .flat_map(|direction| self.neighbor(map, direction))
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

        let (direction_one, direction_two) = pipe.ends();

        let tile_one = self.neighbor(map, direction_one);
        let tile_two = self.neighbor(map, direction_two);

        (tile_one, tile_two)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
enum Enclosure {
    Inside,
    Outside,
}

use Enclosure::*;

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
    pub fn print(
        &self,
        distances_by_tile: &HashMap<(i32, i32), usize>,
        enclosure_by_tile: &HashMap<(i32, i32), Enclosure>,
    ) {
        use std::io::Write;

        let mut buffer = Vec::new();

        let (width, height) = self.tiles.keys().max().copied().unwrap();

        for y in 0..=height {
            for x in 0..=width {
                if let Some(distance) = distances_by_tile.get(&(x, y)) {
                    write!(buffer, "[{distance: >2}]").unwrap();
                } else {
                    let glyph = match enclosure_by_tile.get(&(x, y)) {
                        Some(Inside) => " I",
                        Some(Outside) => " O",
                        None => "..",
                    };

                    write!(buffer, "[{glyph}]").unwrap();
                }
            }

            writeln!(buffer, "").unwrap();
        }

        println!("{}", String::from_utf8(buffer).unwrap());
    }
}

#[adventurous::part_one(answer = "6951")]
pub fn part_one(input: &Input) -> Result<usize> {
    let map = Map::parse(input);
    let distances_by_tile = map.compute_distances();

    #[cfg(test)]
    map.print(&distances_by_tile, &HashMap::new());

    distances_by_tile
        .values()
        .max()
        .copied()
        .ok_or_else(|| anyhow!("no max distance"))
}

#[adventurous::part_two]
pub fn part_two(input: &Input) -> Result<usize> {
    let map = Map::parse(input);
    let distances_by_tile = map.compute_distances();
    let mut inclusion_by_tile: HashMap<(i32, i32), Enclosure> = HashMap::new();

    let (width, height) = map.tiles.keys().max().copied().unwrap();

    let min = distances_by_tile.keys().min().copied().unwrap();
    let max = distances_by_tile.keys().max().copied().unwrap();

    let (min_x, min_y) = min;
    let (max_x, max_y) = max;

    {
        for x in 0..min_x {
            for y in 0..=height {
                inclusion_by_tile.insert((x, y), Outside);
            }
        }

        for x in max_x + 1..=width {
            for y in 0..=height {
                inclusion_by_tile.insert((x, y), Outside);
            }
        }

        for y in 0..min_y {
            for x in 0..=width {
                inclusion_by_tile.insert((x, y), Outside);
            }
        }

        for y in max_y + 1..=height {
            for x in 0..=width {
                inclusion_by_tile.insert((x, y), Outside);
            }
        }
    }

    #[cfg(test)]
    map.print(&distances_by_tile, &inclusion_by_tile);

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
            ...........
            .S-------7.
            .|F-----7|.
            .||.....||.
            .||.....||.
            .|L-7.F-J|.
            .|..|.|..|.
            .L--J.L--J.
            ...........
        "};

        assert_eq!(part_two(&Input::new(input.to_string()))?, 4);

        Ok(())
    }

    #[test]
    #[ignore = "not yet solved"]
    fn test_part_two_sample_input_two() -> Result<()> {
        let input = indoc! {"
            ..........
            .S------7.
            .|F----7|.
            .||....||.
            .||....||.
            .|L-7F-J|.
            .|..||..|.
            .L--JL--J.
            ..........
        "};

        assert_eq!(part_two(&Input::new(input.to_string()))?, 4);

        Ok(())
    }

    #[test]
    #[ignore = "not yet solved"]
    fn test_part_two_sample_input_three() -> Result<()> {
        let input = indoc! {"
            .F----7F7F7F7F-7....
            .|F--7||||||||FJ....
            .||.FJ||||||||L7....
            FJL7L7LJLJ||LJ.L-7..
            L--J.L7...LJS7F-7L7.
            ....F-J..F7FJ|L7L7L7
            ....L7.F7||L7|.L7L7|
            .....|FJLJ|FJ|F7|.LJ
            ....FJL-7.||.||||...
            ....L---J.LJ.LJLJ...
        "};

        assert_eq!(part_two(&Input::new(input.to_string()))?, 8);

        Ok(())
    }

    #[test]
    #[ignore = "not yet solved"]
    fn test_part_two_sample_input_four() -> Result<()> {
        let input = indoc! {"
            FF7FSF7F7F7F7F7F---7
            L|LJ||||||||||||F--J
            FL-7LJLJ||||||LJL-77
            F--JF--7||LJLJ7F7FJ-
            L---JF-JLJ.||-FJLJJ7
            |F|F-JF---7F7-L7L|7|
            |FFJF7L7F-JF7|JL---7
            7-L-JL7||F7|L7F-7F7|
            L.L7LFJ|||||FJL7||LJ
            L7JLJL-JLJLJL--JLJ.L
        "};

        assert_eq!(part_two(&Input::new(input.to_string()))?, 10);

        Ok(())
    }
}
