# Repository Guidelines

Contributor quick-start for OSVM CLI. Keep edits small and well-explained; prefer following the patterns already present in the tree.

## Project Structure & Module Organization
- `src/`: Main CLI code (`main.rs`, `lib.rs`, command modules, services, prompts).
- `crates/`: Supporting Rust crates (`ovsm`, `ovsm-lsp`, `osvm-web`) used by the CLI.
- `examples/ovsm_scripts/` and `completions/`: Sample OVSM programs and AI prompt assets.
- `tests/` and `proptest-regressions/`: Integration tests and regression cases; additional scripts live in `test-scripts/`.
- `assets/` and `public/`: CLI assets, release metadata, and docs.

## Build, Test, and Development Commands
- `make build` / `make build-release`: Debug or optimized build of the CLI.
- `make test`: Run the Rust test suite; equivalent to `cargo test`.
- `make fmt` / `make clippy`: Format with `rustfmt` and lint with `clippy -D warnings`.
- `make run`: Run the debug binary; useful for manual CLI checks.
- `cargo test -p <crate>`: Target a specific crate when iterating inside `crates/`.

## Coding Style & Naming Conventions
- Rust defaults: 4-space indentation, `snake_case` for modules/functions/locals, `CamelCase` for types.
- Always run `cargo fmt` before opening a PR; keep imports ordered by `rustfmt`.
- Treat `clippy` warnings as errors; prefer explicit error handling over `.unwrap()` in user-facing paths.
- Keep module boundaries narrow: helper functions in `utils/`, CLI-facing logic in `commands/`, shared types in `prelude.rs`.

## Testing Guidelines
- Add or update tests under `tests/` for integration behavior; colocate unit tests in the same module using `#[cfg(test)]`.
- Use descriptive test names (`handles_missing_config`, `parses_snapshot_flag`) and exercise CLI arguments where applicable.
- Regenerate property-based regressions in `proptest-regressions/` only when fixing the underlying behavior.
- For OVSM examples, validate with `osvm ovsm check <file>` before committing scripts.

## Commit & Pull Request Guidelines
- Follow the existing Conventional Commit style (`feat(graph): ...`, `fix(cli): ...`, `chore:`).
- In PRs, include: summary of change, linked issue (if any), test commands run (`make test`, `cargo test -p ...`), and any manual verification (`make run` output, example CLI transcripts).
- Keep diffs focused; split large changes into reviewable commits and note breaking changes explicitly.

## Security & Configuration Tips
- Do not commit secrets or keys; prefer environment variables or `.env` entries ignored by git.
- Installation scripts (`install-release.sh`, `install.sh`) assume trusted environments—avoid modifying install paths without confirming downstream impact.
- When adding networked features, guard with flags and ensure defaults remain offline-friendly.
