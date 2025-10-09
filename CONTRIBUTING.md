# Contributing to OSVM

First off, thank you for considering contributing to OSVM! It's people like you that make OSVM such a great tool.

## Code of Conduct

This project and everyone participating in it is governed by our [Code of Conduct](CODE_OF_CONDUCT.md). By participating, you are expected to uphold this code.

## How Can I Contribute?

### Reporting Bugs

Before creating bug reports, please check the issue list as you might find out that you don't need to create one. When you are creating a bug report, please include as many details as possible:

* **Use a clear and descriptive title**
* **Describe the exact steps to reproduce the problem**
* **Provide specific examples to demonstrate the steps**
* **Describe the behavior you observed and what behavior you expected**
* **Include logs and error messages**
* **Note your environment** (OS, Rust version, OSVM version)

### Suggesting Enhancements

Enhancement suggestions are tracked as GitHub issues. When creating an enhancement suggestion, please include:

* **Use a clear and descriptive title**
* **Provide a detailed description of the suggested enhancement**
* **Explain why this enhancement would be useful**
* **List some examples of how it would be used**

### Pull Requests

* Fill in the required template
* Follow the Rust code style (use `cargo fmt`)
* Run `cargo clippy` and address warnings
* Include tests for new functionality
* Update documentation as needed
* Write clear commit messages

## Development Process

### Setting Up Development Environment

```bash
# Clone the repository
git clone https://github.com/opensvm/osvm-cli.git
cd osvm-cli

# Install dependencies
cargo build

# Run tests
cargo test

# Run clippy
cargo clippy -- -D warnings

# Format code
cargo fmt --all
```

### Testing

We maintain high test coverage (98%+). Please ensure:

* All existing tests pass: `cargo test`
* New features include unit tests
* Integration tests are updated if needed
* Run isolation tests: `cargo test --lib isolation`

### Code Style

* Follow Rust standard style guidelines
* Use `cargo fmt` before committing
* Run `cargo clippy` and address all warnings
* Keep functions focused and under 50 lines when possible
* Extract constants for magic numbers
* Write descriptive error messages with fix instructions

### Documentation

* Update README.md if changing user-facing features
* Add rustdoc comments for public APIs
* Update relevant documentation in `docs/`
* Include examples for new features

## Project Structure

See [CLAUDE.md](CLAUDE.md) for detailed architecture documentation.

Key directories:
* `src/` - Main source code
* `src/services/` - High-level services (AI, MCP, Audit)
* `src/utils/` - Core utilities
* `src/utils/isolation/` - Isolation infrastructure (Phases 1-3)
* `examples/` - Example implementations
* `docs/` - Documentation website
* `tests/` - Integration tests

## Git Workflow

1. **Fork** the repository
2. **Create a branch** from `main`:
   ```bash
   git checkout -b feature/amazing-feature
   ```
3. **Make your changes** with clear commit messages:
   ```bash
   git commit -m "feat: add amazing feature"
   ```
4. **Push** to your fork:
   ```bash
   git push origin feature/amazing-feature
   ```
5. **Open a Pull Request** against the `main` branch

### Commit Message Format

Follow conventional commits:

* `feat:` - New feature
* `fix:` - Bug fix
* `docs:` - Documentation changes
* `style:` - Code style changes (formatting)
* `refactor:` - Code refactoring
* `test:` - Adding or updating tests
* `chore:` - Maintenance tasks

Example: `feat: add hot-swap support for validators`

## Isolation Infrastructure Contributions

When working on isolation features (`src/utils/isolation/`):

* Follow the established patterns in existing modules
* Maintain test coverage (98%+ required)
* Document security implications
* Update Architecture.md if changing design
* Coordinate with Phase 4 roadmap items

## Release Process

Releases are managed by maintainers:

1. Version bump in `Cargo.toml`
2. Update `CHANGELOG.md`
3. Create git tag
4. Publish to crates.io
5. Create GitHub release

## Getting Help

* **Documentation**: Check [docs/](docs/) and [CLAUDE.md](CLAUDE.md)
* **Issues**: Search existing issues or create a new one
* **Discussions**: Use GitHub Discussions for questions
* **Discord**: Join our community (link in README)

## Recognition

Contributors are recognized in:
* Git commit history
* GitHub contributors page
* Release notes for significant contributions
* IMPLEMENTATION_COMPLETE.md for phase work

## License

By contributing, you agree that your contributions will be licensed under the MIT License.

---

Thank you for contributing to OSVM! 🚀