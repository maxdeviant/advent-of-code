# Advent of Code

This repository contains my solutions to the [Advent of Code](https://adventofcode.com/) puzzles.

I would highly encourage solving each puzzle on your own before looking at my solutions, lest you deprive yourself of the satisfaction of solving it yourself.

## Directory

| Year           | Language(s)                     |
| -------------- | ------------------------------- |
| [2015](./2015) | [PureScript](./2015/purescript) |
| [2016](./2016) | [PureScript](./2016/purescript) |
| [2017](./2017) | [F#](./2017/fsharp)             |
| [2018](./2018) | [Rust](./2018/rust)             |
| [2019](./2019) | [Haskell](./2019/haskell)       |
| [2020](./2020) | [PureScript](./2020/purescript) |

## My Process

When solving Advent of Code puzzles I try to approach them the way I would in the real world. I tend to take a [domain-driven design](https://en.wikipedia.org/wiki/Domain-driven_design) approach where I model the domain of the puzzle before actually attempting to solve it.

Another thing that you might notice is that in my solutions I always keep the first part of the puzzle working when I implement the second. Again, this is due to my desire to mirror real-world scenarios. Very often you must keep existing functionality working when the requirements change or new functionality is added. For a while I thought that this was a common practice, but as I've looked at other people's solutions it seems that this isn't the case: I've seen many people whose final solver only solves the second part of the puzzle.
