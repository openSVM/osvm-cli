use criterion::{black_box, criterion_group, criterion_main, Criterion};
use ovsm::Scanner;

fn lexer_benchmark(c: &mut Criterion) {
    let source = r#"
        $x = 42
        $y = 10
        $result = $x + $y
    "#;

    c.bench_function("tokenize simple program", |b| {
        b.iter(|| {
            let mut scanner = Scanner::new(black_box(source));
            scanner.scan_tokens().unwrap()
        })
    });
}

criterion_group!(benches, lexer_benchmark);
criterion_main!(benches);
