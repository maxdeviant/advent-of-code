use std::str::FromStr;

use adventurous::Input;

trait Scoring {
    fn get_score(&self) -> i32;
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Shape {
    Rock,
    Paper,
    Scissors,
}

impl Shape {
    fn counter(&self) -> Self {
        match self {
            Shape::Rock => Shape::Paper,
            Shape::Paper => Shape::Scissors,
            Shape::Scissors => Shape::Rock,
        }
    }
}

impl Scoring for Shape {
    fn get_score(&self) -> i32 {
        match self {
            Self::Rock => 1,
            Self::Paper => 2,
            Self::Scissors => 3,
        }
    }
}

impl FromStr for Shape {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "A" | "X" => Ok(Self::Rock),
            "B" | "Y" => Ok(Self::Paper),
            "C" | "Z" => Ok(Self::Scissors),
            _ => Err(format!("'{}' is not a valid shape.", s)),
        }
    }
}

/// The outcome of a [`Round`] from the persective of the active player.
#[derive(Debug)]
enum RoundOutcome {
    Won,
    Lost,
    Draw,
}

impl FromStr for RoundOutcome {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "X" => Ok(Self::Lost),
            "Y" => Ok(Self::Draw),
            "Z" => Ok(Self::Won),
            _ => Err(format!("'{}' is not a valid round outcome.", s)),
        }
    }
}

#[derive(Debug)]
struct Round {
    opponent: Shape,
    counter: Shape,
}

impl Round {
    pub fn outcome(&self) -> RoundOutcome {
        match (&self.counter, &self.opponent) {
            (Shape::Rock, Shape::Scissors)
            | (Shape::Scissors, Shape::Paper)
            | (Shape::Paper, Shape::Rock) => RoundOutcome::Won,
            (Shape::Rock, Shape::Paper)
            | (Shape::Paper, Shape::Scissors)
            | (Shape::Scissors, Shape::Rock) => RoundOutcome::Lost,
            (Shape::Rock, Shape::Rock)
            | (Shape::Paper, Shape::Paper)
            | (Shape::Scissors, Shape::Scissors) => RoundOutcome::Draw,
        }
    }
}

impl Scoring for Round {
    fn get_score(&self) -> i32 {
        let shape_score = self.counter.get_score();
        let round_score = match self.outcome() {
            RoundOutcome::Lost => 0,
            RoundOutcome::Draw => 3,
            RoundOutcome::Won => 6,
        };

        shape_score + round_score
    }
}

impl From<RiggedRound> for Round {
    fn from(rigged: RiggedRound) -> Self {
        let shape_to_choose = match (rigged.desired_outcome, &rigged.opponent) {
            (RoundOutcome::Won, opponent) => opponent.counter(),
            (RoundOutcome::Draw, opponent) => *opponent,
            (RoundOutcome::Lost, opponent) => opponent.counter().counter(),
        };

        Self {
            opponent: rigged.opponent,
            counter: shape_to_choose,
        }
    }
}

impl FromStr for Round {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut moves = s.split(" ").take(2);

        let opponent = moves
            .next()
            .ok_or_else(|| format!("no move for opponent"))
            .and_then(|shape| shape.parse())?;
        let counter = moves
            .next()
            .ok_or_else(|| format!("no move for counter"))
            .and_then(|shape| shape.parse())?;

        Ok(Self { opponent, counter })
    }
}

fn part_one(input: &Input) -> i32 {
    let rounds = input
        .value
        .lines()
        .map(Round::from_str)
        .collect::<Result<Vec<_>, _>>()
        .expect("failed to parse input");

    rounds.into_iter().map(|round| round.get_score()).sum()
}

#[derive(Debug)]
struct RiggedRound {
    opponent: Shape,
    desired_outcome: RoundOutcome,
}

impl FromStr for RiggedRound {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut moves = s.split(" ").take(2);

        let opponent = moves
            .next()
            .ok_or_else(|| format!("no move for opponent"))
            .and_then(|shape| shape.parse())?;
        let desired_outcome = moves
            .next()
            .ok_or_else(|| format!("no desired outcome"))
            .and_then(|shape| shape.parse())?;

        Ok(Self {
            opponent,
            desired_outcome,
        })
    }
}

fn part_two(input: &Input) -> i32 {
    let rounds = input
        .value
        .lines()
        .map(RiggedRound::from_str)
        .map(|result| result.map(Round::from))
        .collect::<Result<Vec<_>, _>>()
        .expect("failed to parse input");

    rounds.into_iter().map(|round| round.get_score()).sum()
}

fn main() -> std::io::Result<()> {
    let input = Input::from_file("input.txt")?;

    println!("Part One: {}", part_one(&input));
    println!("Part Two: {}", part_two(&input));

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one_solution() -> std::io::Result<()> {
        let input = Input::from_file("input.txt")?;

        Ok(assert_eq!(part_one(&input), 14069))
    }

    #[test]
    fn test_part_two_solution() -> std::io::Result<()> {
        let input = Input::from_file("input.txt")?;

        Ok(assert_eq!(part_two(&input), 12411))
    }

    #[test]
    fn test_double_counter() {
        assert_eq!(Shape::Rock.counter().counter(), Shape::Scissors);
        assert_eq!(Shape::Paper.counter().counter(), Shape::Rock);
        assert_eq!(Shape::Scissors.counter().counter(), Shape::Paper);
    }

    #[test]
    fn test_part_one_sample_input() {
        let input = Input {
            value: "A Y\nB X\nC Z".to_string(),
        };

        assert_eq!(part_one(&input), 15);
    }
}
