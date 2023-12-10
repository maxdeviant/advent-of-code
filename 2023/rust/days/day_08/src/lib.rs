mod parser;

use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::sync::{Arc, Mutex, RwLock};
use std::thread::{self, Thread};

use adventurous::Input;
use anyhow::{anyhow, Result};
use crossbeam_channel::{unbounded, Receiver, SendError, Sender};
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

#[derive(Clone)]
struct Ghost {
    pub id: usize,
    pub map: Arc<Map>,
    pub location: Arc<RwLock<Node>>,
    pub instruction_receiver: Receiver<Instruction>,
    pub completion_sender: Sender<(usize, usize)>,
    // pub thread: Thread,
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

// Not: 11567 (too low)
#[adventurous::part_two]
pub fn part_two(input: &Input) -> Result<usize> {
    let map = Arc::new(Map::parse(input)?);

    let start_nodes = map.network.nodes.keys().filter(|node| node.ends_with('A'));

    let (instruction_sender, instruction_receiver) = unbounded();
    let (completion_sender, completion_receiver) = unbounded();

    let mut ghosts = Vec::new();

    for (index, node) in start_nodes.clone().enumerate() {
        let id = index + 1;

        let instruction_receiver = instruction_receiver.clone();
        let completion_sender = completion_sender.clone();

        let ghost = Ghost {
            id,
            map: map.clone(),
            location: Arc::new(RwLock::new(node.clone())),
            instruction_receiver,
            completion_sender,
            // thread: thread.thread().clone(),
        };

        let thread = thread::spawn({
            let ghost = ghost.clone();
            let start_node = node.clone();
            move || {
                let mut instructions = InstructionList::from_iter(ghost.map.instructions.clone());
                let mut location = &start_node;
                // let mut locations = start_nodes.clone().collect::<Vec<_>>();
                let mut steps = 0;

                loop {
                    let instruction = instructions.next().unwrap();

                    location = ghost.map.network.navigate(location, instruction).unwrap();

                    steps += 1;

                    if location.ends_with('Z') {
                        ghost.completion_sender.send((ghost.id, steps)).unwrap();
                    }
                }

                // while let Ok(instruction) = ghost.instruction_receiver.recv() {
                //     // println!("Ghost {} received instruction: {instruction:?}", ghost.id);

                //     let new_location = ghost
                //         .map
                //         .network
                //         .navigate(&ghost.location.read().unwrap(), instruction)
                //         .unwrap();

                //     // println!("Ghost {} moved to {}", ghost.id, new_location);

                //     *ghost.location.write().unwrap() = new_location.clone();
                // }
            }
        });

        ghosts.push(ghost);
    }

    let mut instructions = InstructionList::from_iter(map.instructions.clone());
    // let mut locations = start_nodes.clone().collect::<Vec<_>>();
    let mut steps = 0;

    // thread::spawn(|| {

    // })

    let all_ghost_ids = start_nodes
        .enumerate()
        .map(|(index, _)| index + 1)
        .collect::<Vec<_>>();

    let mut completions: HashMap<usize, HashSet<usize>> = HashMap::new();

    for ghost_id in &all_ghost_ids {
        completions.entry(*ghost_id).or_default();
    }

    while let Ok((ghost_id, steps)) = completion_receiver.recv() {
        let entry = completions.entry(ghost_id).or_default();

        entry.insert(steps);

        let mut sets = completions.values().cloned().collect::<Vec<_>>();

        let (intersection, others) = sets.split_at_mut(1);
        let intersection = &mut intersection[0];
        for other in others {
            intersection.retain(|e| other.contains(e));
        }

        if intersection.len() > 1 {
            dbg!(intersection);
        }

        // if completions.keys().len() == all_ghost_ids.len() {
        //     let min_steps = completions
        //         .values()
        //         .map(|values| values.iter().min().unwrap())
        //         .min()
        //         .unwrap();

        //     println!("Minimum steps = {}", min_steps);

        //     break;
        // }
    }

    // loop {
    //     let instruction = instructions.next().unwrap();

    //     instruction_sender.send(instruction)?;

    //     steps += 1;

    //     dbg!(steps);

    //     // for ghost in ghosts {

    //     // }
    //     // break;

    //     // for ghost in ghosts {
    //     // }

    //     // for location in &mut locations {
    //     //     *location = map.network.navigate(location, instruction)?;
    //     // }

    //     // steps += 1;

    //     let all_ghosts_at_end = ghosts
    //         .iter()
    //         .all(|ghost| ghost.location.read().unwrap().ends_with('Z'));
    //     if all_ghosts_at_end {
    //         break;
    //     }
    // }

    Ok(steps)
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
