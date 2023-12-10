use std::collections::HashMap;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, multispace0, newline, space0};
use nom::combinator::{map, opt};
use nom::error::ParseError;
use nom::multi::many1;
use nom::sequence::delimited;
use nom::{IResult, Parser};

use crate::Instruction::{self, *};
use crate::{Map, Network, Node};

fn whitespace<'a, O, E: ParseError<&'a str>>(
    inner: impl Parser<&'a str, O, E>,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E> {
    delimited(multispace0, inner, multispace0)
}

fn spaces<'a, O, E: ParseError<&'a str>>(
    inner: impl Parser<&'a str, O, E>,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E> {
    delimited(space0, inner, space0)
}

fn node(input: &str) -> IResult<&str, Node> {
    map(alpha1, |str: &str| Node::from(str.to_string()))(input)
}

fn network_entry(input: &str) -> IResult<&str, (Node, (Node, Node))> {
    let (input, origin) = node(input)?;
    let (input, _) = spaces(tag("="))(input)?;
    let (input, _) = spaces(tag("("))(input)?;
    let (input, left) = node(input)?;
    let (input, _) = spaces(tag(","))(input)?;
    let (input, right) = node(input)?;
    let (input, _) = spaces(tag(")"))(input)?;

    Ok((input, (origin, (left, right))))
}

fn network(input: &str) -> IResult<&str, Network> {
    let (input, entries) = many1(delimited(opt(newline), network_entry, opt(newline)))(input)?;

    Ok((
        input,
        Network {
            nodes: HashMap::from_iter(entries),
        },
    ))
}

fn instruction(input: &str) -> IResult<&str, Instruction> {
    let left = map(tag("L"), |_| Left);
    let right = map(tag("R"), |_| Right);

    alt((left, right))(input)
}

fn instructions(input: &str) -> IResult<&str, Vec<Instruction>> {
    many1(instruction)(input)
}

pub(crate) fn parse_map(input: &str) -> IResult<&str, Map> {
    let (input, instructions) = instructions(input)?;
    let (input, network) = whitespace(network)(input)?;

    Ok((
        input,
        Map {
            instructions,
            network,
        },
    ))
}
