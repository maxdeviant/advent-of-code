use adventurous::Input;
use criterion::{criterion_group, criterion_main, Criterion};

use day_03::{part_one, part_two};

fn criterion_benchmark_part_one(crit: &mut Criterion) {
    let input = Input::from(include_str!("../input.txt"));

    let mut group = crit.benchmark_group("day_03");

    group.bench_with_input("part_one", &input, |bencher, input| {
        bencher.iter(|| part_one(input))
    });

    group.finish();
}

fn criterion_benchmark_part_two(crit: &mut Criterion) {
    let input = Input::from(include_str!("../input.txt"));

    let mut group = crit.benchmark_group("day_03");

    group.bench_with_input("part_two", &input, |bencher, input| {
        bencher.iter(|| part_two(input))
    });

    group.finish();
}

criterion_group!(
    benches,
    criterion_benchmark_part_one,
    criterion_benchmark_part_two
);
criterion_main!(benches);
