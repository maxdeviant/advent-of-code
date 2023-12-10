mod parser;

use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::Display;

use adventurous::Input;
use anyhow::{anyhow, Result};
use nom::Finish;

use crate::parser::parse_map;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
enum Instruction {
    Left,
    Right,
}

struct InstructionList {
    instructions: Vec<Instruction>,
    index: usize,
}

impl Iterator for InstructionList {
    type Item = Instruction;

    fn next(&mut self) -> Option<Self::Item> {
        let item = self.instructions[self.index];

        self.index = (self.index + 1) % self.instructions.len();

        Some(item)
    }
}

impl FromIterator<Instruction> for InstructionList {
    fn from_iter<T: IntoIterator<Item = Instruction>>(iter: T) -> Self {
        Self {
            instructions: iter.into_iter().collect(),
            index: 0,
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub struct Node(pub Cow<'static, str>);

impl Node {
    pub const START: Self = Self::from_str("AAA");
    pub const END: Self = Self::from_str("ZZZ");

    pub const fn from_str(value: &'static str) -> Self {
        Self(Cow::Borrowed(value))
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<String> for Node {
    fn from(value: String) -> Self {
        Self(Cow::Owned(value))
    }
}

#[derive(Debug)]
struct Network {
    pub nodes: HashMap<Node, (Node, Node)>,
}

impl Network {
    pub fn navigate(&self, location: &Node, instruction: Instruction) -> Result<&Node> {
        use Instruction::*;

        let (left, right) = self
            .nodes
            .get(&location)
            .ok_or_else(|| anyhow!("location not found: {location}"))?;

        match instruction {
            Left => Ok(left),
            Right => Ok(right),
        }
    }
}

#[derive(Debug)]
struct Map {
    pub instructions: Vec<Instruction>,
    pub network: Network,
}

impl Map {
    pub fn parse(input: &Input) -> Result<Self> {
        let (_, map) = parse_map(input.raw())
            .finish()
            .map_err(|err| nom::error::Error {
                code: err.code,
                input: err.input.to_string(),
            })?;
        Ok(map)
    }
}

#[adventurous::part_one(answer = "15871")]
pub fn part_one(input: &Input) -> Result<usize> {
    let map = Map::parse(input)?;

    let mut instructions = InstructionList::from_iter(map.instructions);
    let mut location = &Node::START;
    let mut steps = 0;

    while location != &Node::END {
        let instruction = instructions.next().unwrap();

        location = map.network.navigate(location, instruction)?;

        steps += 1;
    }

    Ok(steps)
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
            RL

            AAA = (BBB, CCC)
            BBB = (DDD, EEE)
            CCC = (ZZZ, GGG)
            DDD = (DDD, DDD)
            EEE = (EEE, EEE)
            GGG = (GGG, GGG)
            ZZZ = (ZZZ, ZZZ)
        "};

        assert_eq!(part_one(&Input::new(input.to_string()))?, 2);

        Ok(())
    }

    #[test]
    fn test_part_one_sample_input_two() -> Result<()> {
        let input = indoc! {"
            LLR

            AAA = (BBB, BBB)
            BBB = (AAA, ZZZ)
            ZZZ = (ZZZ, ZZZ)
        "};

        assert_eq!(part_one(&Input::new(input.to_string()))?, 6);

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

    #[test]
    fn test_instruction_list_repetition() {
        use Instruction::*;

        let mut instructions = InstructionList::from_iter(vec![Left, Left, Right]);

        assert_eq!(instructions.next(), Some(Left));
        assert_eq!(instructions.next(), Some(Left));
        assert_eq!(instructions.next(), Some(Right));
        assert_eq!(instructions.next(), Some(Left));
        assert_eq!(instructions.next(), Some(Left));
        assert_eq!(instructions.next(), Some(Right));
    }
}
