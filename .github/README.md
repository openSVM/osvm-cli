# GitHub Actions Workflows for OSVM CLI

This directory contains GitHub Actions workflow proposals for the OSVM CLI project. These workflows automate testing, benchmarking, and deployment processes.

## Workflows

### CI (Continuous Integration)

The CI workflow (`workflows/ci.yml`) runs on every push to the main branch and on pull requests. It performs:

1. **Sanity Checks**: Runs `rustfmt` and `clippy` to ensure code quality and style.
2. **Unit Tests**: Runs unit tests for the library and binary components.
3. **End-to-End Tests**: Runs the end-to-end tests that verify CLI functionality.
4. **Code Coverage**: Generates a code coverage report using `cargo-tarpaulin` and uploads it to Codecov.

### Cross-Platform Tests

The Cross-Platform Tests workflow (`workflows/cross-platform.yml`) runs on every push to the main branch and on pull requests. It:

1. Builds and tests the application on multiple operating systems:
   - Ubuntu Linux
   - macOS
   - Windows
2. Ensures the CLI works consistently across different platforms.

### Benchmarks

The Benchmarks workflow (`workflows/benchmarks.yml`) runs on every push to the main branch, on pull requests, and weekly on Sundays. It:

1. Runs performance benchmarks using `cargo-criterion`.
2. Uploads benchmark results as artifacts.
3. Generates a benchmark report.
4. For pull requests, compares benchmarks with the main branch to detect performance regressions.

### Security Scan

The Security Scan workflow (`workflows/security.yml`) runs on every push to the main branch, on pull requests, and weekly on Mondays. It:

1. **Security Audit**: Runs `cargo-audit` to check for known vulnerabilities in dependencies.
2. **Dependency Review**: Reviews dependencies for security issues in pull requests.
3. **Code Scanning**: Uses GitHub's CodeQL to scan for security vulnerabilities in the code.

### Release

The Release workflow (`workflows/release.yml`) runs when a tag starting with 'v' is pushed. It:

1. **Builds** the release binary.
2. **Creates a GitHub Release** with the binary attached.
3. **Deploys to APT Repository**: Creates a Debian package and deploys it to an APT repository.
4. **Deploys to Homebrew**: Creates a Homebrew formula and submits it to Homebrew.
5. **Deploys Documentation**: Generates documentation using `cargo doc` and deploys it to GitHub Pages.

## Usage

To use these workflows:

1. Move the `.github-proposal` directory to `.github` in your repository.
2. Customize the workflows as needed for your specific requirements.
3. For the Release workflow, you'll need to set up:
   - An APT repository for Debian package deployment
   - A Homebrew tap for formula submission
   - GitHub Pages for documentation hosting

## Requirements

These workflows require:

- GitHub Actions enabled on your repository
- Appropriate permissions for the GitHub token
- For the Release workflow, additional secrets may be needed for deployment

## Customization

You can customize these workflows by:

- Adjusting the triggers (e.g., which branches to run on)
- Adding or removing steps
- Changing the deployment targets
- Modifying the build parameters

## Troubleshooting

If you encounter issues with these workflows:

- Check the GitHub Actions logs for detailed error messages
- Ensure all required secrets are properly set
- Verify that the repository has the necessary permissions