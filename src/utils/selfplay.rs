use anyhow::{Context, Result};
use rand::Rng;
use serde::{Deserialize, Serialize};
use std::fs::{self, OpenOptions};
use std::io::Write;
use std::path::PathBuf;
use std::time::{SystemTime, UNIX_EPOCH};

#[derive(Debug, Serialize, Deserialize)]
struct SelfPlaySeed {
    task: String,
    ovsm_plan: String,
    expected_result: String,
    difficulty: u32,
    timestamp: u64,
    source_query: String,
}

fn seeds_path() -> PathBuf {
    dirs::home_dir()
        .unwrap_or_else(|| PathBuf::from("."))
        .join(".osvm")
        .join("selfplay")
        .join("seeds.jsonl")
}

fn now_ts() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0)
}

fn should_skip_run(seeds: &[SelfPlaySeed]) -> bool {
    const MAX_SEEDS: usize = 200;
    const MIN_INTERVAL_SECS: u64 = 60;

    if seeds.len() >= MAX_SEEDS {
        return true;
    }

    if let Some(last) = seeds.last() {
        let elapsed = now_ts().saturating_sub(last.timestamp);
        return elapsed < MIN_INTERVAL_SECS;
    }

    false
}

fn read_seeds() -> Vec<SelfPlaySeed> {
    let path = seeds_path();
    let content = fs::read_to_string(path).unwrap_or_default();
    content
        .lines()
        .filter_map(|line| serde_json::from_str::<SelfPlaySeed>(line).ok())
        .collect()
}

fn write_seed(seed: &SelfPlaySeed) -> Result<()> {
    let path = seeds_path();
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).context("creating selfplay directory")?;
    }

    let mut file = OpenOptions::new()
        .create(true)
        .append(true)
        .open(&path)
        .with_context(|| format!("opening selfplay seeds file at {}", path.display()))?;

    let line = serde_json::to_string(seed)?;
    writeln!(file, "{}", line)?;
    Ok(())
}

fn generate_task(difficulty: u32) -> (String, String, String) {
    let mut rng = rand::rng();

    match difficulty {
        0 | 1 => {
            let a: u64 = rng.random_range(2..50);
            let b: u64 = rng.random_range(2..50);
            let task = format!("Add two integers: {} + {}", a, b);
            let plan = format!(
                ";; Self-play OVSM plan (difficulty 1)\n(let ((a {}) (b {})) (+ a b))",
                a, b
            );
            let result = (a + b).to_string();
            (task, plan, result)
        }
        2 => {
            let n: u64 = rng.random_range(3..8);
            let task = format!("Compute factorial of {}", n);
            let plan = format!(
                ";; Self-play OVSM plan (difficulty 2)\n\
                 (define (fact n)\n  (if (<= n 1) 1 (* n (fact (- n 1)))))\n\
                 (fact {})",
                n
            );
            let result = (1..=n).product::<u64>().to_string();
            (task, plan, result)
        }
        3 => {
            let n: u64 = rng.random_range(4..7);
            let task = format!("Sum the first {} squares", n);
            let plan = format!(
                ";; Self-play OVSM plan (difficulty 3)\n\
                 (define (sum-squares n)\n\
                   (define (loop i acc)\n\
                     (if (> i n) acc (loop (+ i 1) (+ acc (* i i)))))\n\
                   (loop 1 0))\n\
                 (sum-squares {})",
                n
            );
            let result = (1..=n).map(|i| i * i).sum::<u64>().to_string();
            (task, plan, result)
        }
        _ => {
            let n: u64 = rng.random_range(4..8);
            let task = format!("Compute the nth Fibonacci where n={}", n);
            let plan = format!(
                ";; Self-play OVSM plan (difficulty 4+)\n\
                 (define (fib n)\n\
                   (if (<= n 1)\n\
                       n\n\
                       (+ (fib (- n 1)) (fib (- n 2)))))\n\
                 (fib {})",
                n
            );
            let result = {
                let mut a = 0u64;
                let mut b = 1u64;
                for _ in 0..n {
                    let next = a + b;
                    a = b;
                    b = next;
                }
                a
            }
            .to_string();
            (task, plan, result)
        }
    }
}

async fn run_selfplay_once(source_query: &str) -> Result<()> {
    let seeds = read_seeds();
    if should_skip_run(&seeds) {
        return Ok(());
    }

    let difficulty = (seeds.len() as u32 % 5).max(1);
    let (task, ovsm_plan, expected_result) = generate_task(difficulty);

    let seed = SelfPlaySeed {
        task,
        ovsm_plan,
        expected_result,
        difficulty,
        timestamp: now_ts(),
        source_query: source_query.to_string(),
    };

    write_seed(&seed)?;
    Ok(())
}

/// Spawn a background self-play iteration without blocking user requests.
pub fn spawn_background_selfplay(source_query: &str) {
    let query = source_query.to_string();
    tokio::spawn(async move {
        if let Err(e) = run_selfplay_once(&query).await {
            log::debug!("self-play iteration skipped: {}", e);
        }
    });
}
