use std::hint::black_box;

use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use serde_json::{json as value, Value};

fn build_evaluator() -> jexl_eval::Evaluator<'static> {
    jexl_3000::build_evaluator()
}

fn bench_string_range(c: &mut Criterion) {
    let mut group = c.benchmark_group("string_range");
    let evaluator = build_evaluator();

    // Test string range with step - exercises the += bug fix
    let sizes = vec![10, 50, 100, 500];
    for size in sizes {
        let string = "a".repeat(size);
        let context = value!({ "str": string });

        group.bench_with_input(BenchmarkId::new("with_step", size), &size, |b, _| {
            b.iter(|| {
                evaluator
                    .eval_in_context(black_box("str | range(0, -1, 2)"), black_box(&context))
                    .unwrap()
            });
        });
    }
    group.finish();
}

fn bench_array_unique_numbers(c: &mut Criterion) {
    let mut group = c.benchmark_group("array_unique_numbers");
    let evaluator = build_evaluator();

    // Test numeric unique - exercises the roundtrip elimination
    let sizes = vec![10, 50, 100, 500, 1000];
    for size in sizes {
        // Create array with duplicates: [1, 2, 3, ..., n/2, 1, 2, 3, ..., n/2]
        let mut array: Vec<Value> = (1..=(size / 2)).map(|i| value!(i as f64)).collect();
        array.extend((1..=(size / 2)).map(|i| value!(i as f64)));
        let context = value!({ "arr": array });

        group.bench_with_input(BenchmarkId::new("with_duplicates", size), &size, |b, _| {
            b.iter(|| {
                evaluator
                    .eval_in_context(black_box("arr | unique"), black_box(&context))
                    .unwrap()
            });
        });
    }
    group.finish();
}

fn bench_array_unique_strings(c: &mut Criterion) {
    let mut group = c.benchmark_group("array_unique_strings");
    let evaluator = build_evaluator();

    // Test string unique - exercises the optimized unique() function
    let sizes = vec![10, 50, 100, 500, 1000];
    for size in sizes {
        let mut array: Vec<Value> = (0..(size / 2))
            .map(|i| value!(format!("item_{}", i)))
            .collect();
        array.extend((0..(size / 2)).map(|i| value!(format!("item_{}", i))));
        let context = value!({ "arr": array });

        group.bench_with_input(BenchmarkId::new("with_duplicates", size), &size, |b, _| {
            b.iter(|| {
                evaluator
                    .eval_in_context(black_box("arr | unique"), black_box(&context))
                    .unwrap()
            });
        });
    }
    group.finish();
}

fn bench_array_sort(c: &mut Criterion) {
    let mut group = c.benchmark_group("array_sort");
    let evaluator = build_evaluator();

    // Test array sort - exercises unstable sort
    let sizes = vec![10, 50, 100, 500, 1000];
    for &size in &sizes {
        // Create reverse-sorted array
        let array: Vec<Value> = (0..size).rev().map(|i| value!(i as f64)).collect();
        let context = value!({ "arr": array });

        group.bench_with_input(BenchmarkId::new("numbers", size), &size, |b, _| {
            b.iter(|| {
                evaluator
                    .eval_in_context(black_box("arr | sort"), black_box(&context))
                    .unwrap()
            });
        });
    }

    // String sort
    for &size in &sizes {
        let array: Vec<Value> = (0..size)
            .rev()
            .map(|i| value!(format!("item_{:04}", i)))
            .collect();
        let context = value!({ "arr": array });

        group.bench_with_input(BenchmarkId::new("strings", size), &size, |b, _| {
            b.iter(|| {
                evaluator
                    .eval_in_context(black_box("arr | sort"), black_box(&context))
                    .unwrap()
            });
        });
    }
    group.finish();
}

fn bench_array_filter(c: &mut Criterion) {
    let mut group = c.benchmark_group("array_filter");
    let evaluator = build_evaluator();

    // Test array filter - will benefit from Phase 2 context optimization
    let sizes = vec![10, 50, 100, 500, 1000];
    for size in sizes {
        let array: Vec<Value> = (0..size)
            .map(|i| value!({ "value": i, "active": i % 2 == 0 }))
            .collect();
        let context = value!({ "items": array });

        group.bench_with_input(BenchmarkId::new("simple_filter", size), &size, |b, _| {
            b.iter(|| {
                evaluator
                    .eval_in_context(black_box("items[.active]"), black_box(&context))
                    .unwrap()
            });
        });
    }
    group.finish();
}

fn bench_sort_by_attribute(c: &mut Criterion) {
    let mut group = c.benchmark_group("sort_by_attribute");
    let evaluator = build_evaluator();

    // Test sortByAttribute - exercises unstable sort
    let sizes = vec![10, 50, 100, 500];
    for size in sizes {
        let array: Vec<Value> = (0..size)
            .rev()
            .map(|i| value!({ "id": i, "name": format!("item_{}", i) }))
            .collect();
        let context = value!({ "items": array });

        group.bench_with_input(BenchmarkId::new("numeric_field", size), &size, |b, _| {
            b.iter(|| {
                evaluator
                    .eval_in_context(
                        black_box("items | sortByAttribute('id')"),
                        black_box(&context),
                    )
                    .unwrap()
            });
        });
    }
    group.finish();
}

fn bench_complex_transforms(c: &mut Criterion) {
    let mut group = c.benchmark_group("complex_transforms");
    let evaluator = build_evaluator();

    // Test complex transform chains - exercises value cloning
    let size = 100;
    let array: Vec<Value> = (0..size)
        .map(|i| value!({ "value": i, "active": i % 3 == 0 }))
        .collect();
    let context = value!({ "items": array });

    group.bench_function("filter_map_sort", |b| {
        b.iter(|| {
            evaluator
                .eval_in_context(
                    black_box("items[.active] | pick('value') | sort"),
                    black_box(&context),
                )
                .unwrap()
        });
    });

    group.finish();
}

criterion_group!(
    benches,
    bench_string_range,
    bench_array_unique_numbers,
    bench_array_unique_strings,
    bench_array_sort,
    bench_array_filter,
    bench_sort_by_attribute,
    bench_complex_transforms
);
criterion_main!(benches);
