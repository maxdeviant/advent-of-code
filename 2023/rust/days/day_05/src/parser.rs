use indexmap::IndexMap;
use nom::bytes::complete::{tag, take_until, take_while1};
use nom::character::complete::{multispace0, newline, space0};
use nom::combinator::{map, map_res, opt};
use nom::error::ParseError;
use nom::multi::many1;
use nom::sequence::{delimited, terminated};
use nom::{IResult, Parser};

use crate::{Almanac, Map, MapRange, Seed};

fn whitespace<'a, O, E: ParseError<&'a str>>(
    inner: impl Parser<&'a str, O, E>,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E> {
    delimited(multispace0, inner, multispace0)
}

fn colon(input: &str) -> IResult<&str, &str> {
    tag(":")(input)
}

fn number(input: &str) -> IResult<&str, usize> {
    map_res(
        take_while1(|char: char| char.is_ascii_digit()),
        |n: &str| n.parse::<usize>(),
    )(input)
}

fn range(input: &str) -> IResult<&str, MapRange> {
    let (input, destination_start) = terminated(number, space0)(input)?;
    let (input, source_start) = terminated(number, space0)(input)?;
    let (input, length) = terminated(number, space0)(input)?;

    Ok((
        input,
        MapRange {
            destination_start,
            source_start,
            length,
        },
    ))
}

fn map_name(input: &str) -> IResult<&str, &str> {
    take_until(":")(input)
}

fn almanac_map(input: &str) -> IResult<&str, Map> {
    let (input, name) = map_name(input)?;
    let (input, _) = colon(input)?;
    let (input, _) = newline(input)?;
    let (input, ranges) = many1(delimited(opt(newline), range, opt(newline)))(input)?;

    Ok((
        input,
        Map {
            name: name.to_owned(),
            ranges,
        },
    ))
}

fn seeds(input: &str) -> IResult<&str, Vec<Seed>> {
    let (input, _) = whitespace(tag("seeds:"))(input)?;

    many1(terminated(map(number, Seed), opt(tag(" "))))(input)
}

pub(crate) fn parse_almanac(input: &str) -> IResult<&str, Almanac> {
    let (input, seeds) = seeds(input)?;
    let (input, maps) = many1(whitespace(almanac_map))(input)?;

    let maps = maps
        .into_iter()
        .map(|map| (map.name.clone(), map))
        .collect::<IndexMap<_, _>>();

    Ok((input, Almanac { seeds, maps }))
}
