mod parser;

use std::borrow::Cow;
use std::fmt::Display;

use adventurous::Input;
use anyhow::{anyhow, Result};
use indexmap::IndexMap;
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

    pub fn ends_with(&self, char: char) -> bool {
        self.0.ends_with(char)
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

trait IsEndNode {
    fn is_end(&self, node: &Node) -> bool;
}

impl IsEndNode for &Node {
    fn is_end(&self, node: &Node) -> bool {
        *self == node
    }
}

impl<F> IsEndNode for F
where
    F: Fn(&Node) -> bool,
{
    fn is_end(&self, node: &Node) -> bool {
        self(node)
    }
}

#[derive(Debug)]
struct Network {
    pub nodes: IndexMap<Node, (Node, Node)>,
}

impl Network {
    pub fn navigate(&self, location: &Node, instruction: Instruction) -> Result<&Node> {
        use Instruction::*;

        let (left, right) = self
            .nodes
            .get(location)
            .ok_or_else(|| anyhow!("location not found: {location}"))?;

        match instruction {
            Left => Ok(left),
            Right => Ok(right),
        }
    }

    pub fn navigate_to_end(
        &self,
        start: &Node,
        end: impl IsEndNode,
        instructions: &mut InstructionList,
    ) -> Result<usize> {
        let mut steps = 0;
        let mut location = start;

        loop {
            let instruction = instructions.next().unwrap();

            location = self.navigate(location, instruction)?;

            steps += 1;

            if end.is_end(&location) {
                return Ok(steps);
            }
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

    map.network
        .navigate_to_end(&Node::START, &Node::END, &mut instructions)
}

#[adventurous::part_two(answer = "11283670395017")]
pub fn part_two(input: &Input) -> Result<usize> {
    let map = Map::parse(input)?;
    let mut instructions = InstructionList::from_iter(map.instructions);

    let start_nodes = map.network.nodes.keys().filter(|node| node.ends_with('A'));

    let steps_to_end = start_nodes
        .map(|start_node| {
            map.network.navigate_to_end(
                &start_node,
                |node: &Node| node.ends_with('Z'),
                &mut instructions,
            )
        })
        .collect::<Result<Vec<_>, _>>()?;

    steps_to_end
        .into_iter()
        .reduce(num::integer::lcm)
        .ok_or_else(|| anyhow!("no overlap between ghosts"))
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
    fn test_part_two_sample_input() -> Result<()> {
        let input = indoc! {"
            LR

            11A = (11B, XXX)
            11B = (XXX, 11Z)
            11Z = (11B, XXX)
            22A = (22B, XXX)
            22B = (22C, 22C)
            22C = (22Z, 22Z)
            22Z = (22B, 22B)
            XXX = (XXX, XXX)
        "};

        assert_eq!(part_two(&Input::new(input.to_string()))?, 6);

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
