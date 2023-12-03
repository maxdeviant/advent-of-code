use nom::bytes::complete::{tag, take_till, take_until};
use nom::character::complete::multispace0;
use nom::combinator::{map_res, opt};
use nom::error::ParseError;
use nom::multi::many1;
use nom::sequence::{delimited, terminated};
use nom::{IResult, Parser};

use crate::{
    Cube::{self, *},
    Game,
};

fn whitespace<'a, O, E: ParseError<&'a str>>(
    inner: impl Parser<&'a str, O, E>,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E> {
    delimited(multispace0, inner, multispace0)
}

fn colon(input: &str) -> IResult<&str, &str> {
    tag(":")(input)
}

fn till_colon(input: &str) -> IResult<&str, &str> {
    take_until(":")(input)
}

fn cube(input: &str) -> IResult<&str, Cube> {
    map_res(
        whitespace(take_till(|char: char| !char.is_alphabetic())),
        |parsed| match parsed {
            "red" => Ok(Red),
            "green" => Ok(Green),
            "blue" => Ok(Blue),
            _ => Err(format!("unknown cube color: '{parsed}'")),
        },
    )(input)
}

fn cube_pair(input: &str) -> IResult<&str, (Cube, usize)> {
    let (input, count) = map_res(
        whitespace(take_till(|char: char| !char.is_ascii_digit())),
        |char| char.parse::<usize>(),
    )(input)?;
    let (input, cube) = cube(input)?;

    Ok((input, (cube, count)))
}

fn cube_subsets(input: &str) -> IResult<&str, Vec<(Cube, usize)>> {
    many1(terminated(cube_pair, whitespace(opt(tag(",")))))(input)
}

fn game_sets(input: &str) -> IResult<&str, Vec<Vec<(Cube, usize)>>> {
    many1(terminated(cube_subsets, whitespace(opt(tag(";")))))(input)
}

fn game_id(input: &str) -> IResult<&str, usize> {
    map_res(till_colon, |char| char.parse::<usize>())(input)
}

pub fn parse_game(input: &str) -> IResult<&str, Game> {
    let (input, _) = whitespace(tag("Game"))(input)?;
    let (input, id) = game_id(input)?;
    let (input, _) = whitespace(colon)(input)?;
    let (input, sets) = game_sets(input)?;

    Ok((input, Game { id, sets }))
}
