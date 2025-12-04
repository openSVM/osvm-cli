use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use ovsm::runtime::LispEvaluator;

/// Benchmark sequential vs concurrent execution
fn bench_sequential_vs_concurrent(c: &mut Criterion) {
    let mut group = c.benchmark_group("sequential_vs_concurrent");

    for size in [10, 50, 100].iter() {
        // Sequential benchmark
        group.bench_with_input(BenchmarkId::new("sequential", size), size, |b, &size| {
            b.iter(|| {
                let mut evaluator = LispEvaluator::new();
                let code = format!(
                    r#"
(do
  (defun compute (x) (* x x))

  (define results [])
  (for (i (range 1 {}))
    (set! results (append results [(compute i)])))

  (length results))
"#,
                    size + 1
                );

                black_box(evaluator.eval_str(&code).unwrap())
            });
        });

        // Concurrent benchmark
        group.bench_with_input(BenchmarkId::new("concurrent", size), size, |b, &size| {
            b.iter(|| {
                let mut evaluator = LispEvaluator::new();
                let code = format!(
                    r#"
(do
  (defun compute (x) (* x x))

  (define handles [])
  (for (i (range 1 {}))
    (set! handles (append handles [(async compute i)])))

  (define results [])
  (for (h handles)
    (set! results (append results [(await h)])))

  (length results))
"#,
                    size + 1
                );

                black_box(evaluator.eval_str(&code).unwrap())
            });
        });
    }

    group.finish();
}

/// Benchmark task creation overhead
fn bench_async_overhead(c: &mut Criterion) {
    let mut group = c.benchmark_group("async_overhead");

    group.bench_function("create_handle", |b| {
        b.iter(|| {
            let mut evaluator = LispEvaluator::new();
            let code = r#"
(do
  (defun noop (x) x)
  (async noop 42))
"#;
            black_box(evaluator.eval_str(code).unwrap())
        });
    });

    group.bench_function("create_and_await", |b| {
        b.iter(|| {
            let mut evaluator = LispEvaluator::new();
            let code = r#"
(do
  (defun noop (x) x)
  (define h (async noop 42))
  (await h))
"#;
            black_box(evaluator.eval_str(code).unwrap())
        });
    });

    group.finish();
}

/// Benchmark batch processing
fn bench_batch_processing(c: &mut Criterion) {
    let mut group = c.benchmark_group("batch_processing");

    for batch_size in [10, 50, 100].iter() {
        group.bench_with_input(
            BenchmarkId::new("concurrent_batch", batch_size),
            batch_size,
            |b, &size| {
                b.iter(|| {
                    let mut evaluator = LispEvaluator::new();
                    let code = format!(
                        r#"
(do
  (defun process (x)
    (do
      (sleep 5)
      (* x 2)))

  (define handles [])
  (for (i (range 1 {}))
    (set! handles (append handles [(async process i)])))

  (define results [])
  (for (h handles)
    (set! results (append results [(await h)])))

  (length results))
"#,
                        size + 1
                    );

                    black_box(evaluator.eval_str(&code).unwrap())
                });
            },
        );
    }

    group.finish();
}

/// Benchmark map-reduce pattern
fn bench_map_reduce(c: &mut Criterion) {
    c.bench_function("map_reduce_100_items", |b| {
        b.iter(|| {
            let mut evaluator = LispEvaluator::new();
            let code = r#"
(do
  (defun mapper (chunk-id data-chunk)
    (do
      (define sum 0)
      (for (val data-chunk)
        (set! sum (+ sum val)))
      {:chunk-id chunk-id :sum sum}))

  (define data (range 1 101))
  (define chunks [
    (take 25 data)
    (take 25 (drop 25 data))
    (take 25 (drop 50 data))
    (take 25 (drop 75 data))
  ])

  ;; Map phase
  (define handles [])
  (define chunk-id 0)
  (for (chunk chunks)
    (do
      (set! handles (append handles [(async mapper chunk-id chunk)]))
      (set! chunk-id (+ chunk-id 1))))

  (define map-results [])
  (for (h handles)
    (set! map-results (append map-results [(await h)])))

  ;; Reduce phase
  (define total 0)
  (for (r map-results)
    (set! total (+ total (get r :sum))))

  total)
"#;

            black_box(evaluator.eval_str(code).unwrap())
        });
    });
}

/// Benchmark factorial computation (CPU-intensive)
fn bench_factorial_concurrent(c: &mut Criterion) {
    c.bench_function("factorial_concurrent_10_tasks", |b| {
        b.iter(|| {
            let mut evaluator = LispEvaluator::new();
            let code = r#"
(do
  (defun factorial (n)
    (if (<= n 1)
        1
        (* n (factorial (- n 1)))))

  (define handles [])
  (for (n (range 10 21))
    (set! handles (append handles [(async factorial n)])))

  (define results [])
  (for (h handles)
    (set! results (append results [(await h)])))

  (length results))
"#;

            black_box(evaluator.eval_str(code).unwrap())
        });
    });
}

/// Benchmark fire-and-forget pattern
fn bench_fire_and_forget(c: &mut Criterion) {
    c.bench_function("fire_and_forget_100_tasks", |b| {
        b.iter(|| {
            let mut evaluator = LispEvaluator::new();
            let code = r#"
(do
  (defun background-work (id)
    (do
      (sleep 1)
      (* id 2)))

  (for (i (range 1 101))
    (async background-work i))

  "done")
"#;

            black_box(evaluator.eval_str(code).unwrap())
        });
    });
}

criterion_group!(
    benches,
    bench_sequential_vs_concurrent,
    bench_async_overhead,
    bench_batch_processing,
    bench_map_reduce,
    bench_factorial_concurrent,
    bench_fire_and_forget
);
criterion_main!(benches);
