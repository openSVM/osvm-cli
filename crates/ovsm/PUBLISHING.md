# Publishing OVSM to crates.io

This guide explains how to publish the OVSM crate to crates.io.

## Prerequisites

1. **Crates.io Account**
   - Create account at https://crates.io
   - Generate API token at https://crates.io/me

2. **GitHub Secret**
   - Add `CARGO_REGISTRY_TOKEN` to repository secrets
   - Settings → Secrets and variables → Actions → New repository secret

3. **Cargo.toml Verification**
   - Ensure all metadata is correct (version, description, license, etc.)
   - Verify dependencies are properly specified

## Publishing Methods

### Method 1: Automatic via GitHub Actions (Recommended)

#### Step 1: Update Version

Edit `crates/ovsm/Cargo.toml`:

```toml
[package]
version = "1.0.1"  # Increment version
```

#### Step 2: Update Changelog

Edit `crates/ovsm/CHANGELOG.md`:

```markdown
## [1.0.1] - 2025-10-11

### Added
- New feature X
- New feature Y

### Fixed
- Bug fix A
- Bug fix B
```

#### Step 3: Commit Changes

```bash
git add crates/ovsm/Cargo.toml crates/ovsm/CHANGELOG.md
git commit -m "chore(ovsm): bump version to 1.0.1"
git push origin main
```

#### Step 4: Create Git Tag

```bash
git tag ovsm-v1.0.1
git push origin ovsm-v1.0.1
```

This will automatically trigger the GitHub Actions workflow that:
1. ✅ Runs all tests
2. ✅ Verifies formatting and linting
3. ✅ Tests all examples
4. ✅ Publishes to crates.io
5. ✅ Creates GitHub Release
6. ✅ Deploys documentation

#### Step 5: Verify Publication

Check:
- https://crates.io/crates/ovsm
- https://docs.rs/ovsm
- https://github.com/opensvm/osvm-cli/releases

---

### Method 2: Manual Publication

#### Step 1: Verify Everything Works

```bash
cd crates/ovsm

# Check formatting
cargo fmt --all -- --check

# Run clippy
cargo clippy --all-targets --all-features -- -D warnings

# Run all tests
cargo test

# Test examples
cargo run --example run_file examples/hello_world.ovsm
```

#### Step 2: Verify Package

```bash
# Dry run to check what will be published
cargo package --allow-dirty

# Check the generated package
cd target/package
tar -xzf ovsm-*.crate
ls -la ovsm-*/
```

#### Step 3: Publish

```bash
cd crates/ovsm
cargo publish --token YOUR_TOKEN_HERE
```

Or with token from environment:

```bash
export CARGO_REGISTRY_TOKEN="your-token-here"
cargo publish
```

---

### Method 3: Dry Run (Testing Only)

Test the publishing workflow without actually publishing:

```bash
# Via GitHub Actions UI
# Go to Actions → Publish OVSM Crate → Run workflow
# Set "Dry run" to "true"

# Or manually
cd crates/ovsm
cargo publish --dry-run --allow-dirty
```

---

## Version Numbering

Follow [Semantic Versioning](https://semver.org/):

- **MAJOR** version (1.x.x → 2.0.0): Breaking changes
- **MINOR** version (1.0.x → 1.1.0): New features, backward compatible
- **PATCH** version (1.0.0 → 1.0.1): Bug fixes, backward compatible

### Examples

```toml
# Bug fix release
version = "1.0.0" → "1.0.1"

# New feature (backward compatible)
version = "1.0.1" → "1.1.0"

# Breaking changes
version = "1.1.0" → "2.0.0"
```

---

## Pre-Release Checklist

Before publishing, verify:

- [ ] All tests pass: `cargo test`
- [ ] Examples work: Test all `.ovsm` scripts
- [ ] Documentation builds: `cargo doc --no-deps`
- [ ] Package builds: `cargo package --allow-dirty`
- [ ] Version bumped in `Cargo.toml`
- [ ] `CHANGELOG.md` updated
- [ ] `README.md` accurate
- [ ] License file present (`LICENSE` or `LICENSE-MIT`)
- [ ] No sensitive data in repository

---

## Post-Release Tasks

After successful publication:

1. **Verify on crates.io**
   ```
   https://crates.io/crates/ovsm/1.0.1
   ```

2. **Check documentation**
   ```
   https://docs.rs/ovsm/1.0.1
   ```

3. **Test installation**
   ```bash
   cargo install ovsm --version 1.0.1
   ```

4. **Announce release**
   - Update main README.md
   - Social media / Discord / Telegram
   - Release notes on GitHub

5. **Monitor for issues**
   - Check GitHub issues
   - Monitor crates.io download stats

---

## Troubleshooting

### Error: Version Already Published

**Problem:** `error: crate version 1.0.0 is already uploaded`

**Solution:** Bump version in `Cargo.toml` - you cannot re-publish the same version.

```toml
version = "1.0.1"  # Increment version
```

---

### Error: Missing License File

**Problem:** `error: missing license file`

**Solution:** Ensure `LICENSE` or `LICENSE-MIT` file exists:

```bash
cd crates/ovsm
# If missing, copy from root
cp ../../LICENSE .
```

---

### Error: Documentation Build Failed

**Problem:** `cargo doc` fails

**Solution:** Fix documentation errors:

```bash
# Check what's wrong
cargo doc --no-deps

# Common issues:
# - Missing doc comments on public items
# - Broken doc links
# - Code examples that don't compile
```

---

### Error: Package Too Large

**Problem:** `error: package size exceeds limit`

**Solution:** Exclude unnecessary files in `Cargo.toml`:

```toml
[package]
exclude = [
    "tests/fixtures/*",
    "benches/data/*",
    "docs/*",
    "*.md",
    "!README.md",
    "!CHANGELOG.md",
]
```

---

### Error: Authentication Failed

**Problem:** `error: authentication failed`

**Solution:** Check your token:

```bash
# Verify token is set
echo $CARGO_REGISTRY_TOKEN

# Or use explicit token
cargo publish --token YOUR_TOKEN_HERE

# Or login interactively
cargo login
```

---

## Yanking a Release

If you need to remove a published version:

```bash
# Yank version (prevents new projects from using it)
cargo yank --vers 1.0.1

# Unyank if you change your mind
cargo yank --vers 1.0.1 --undo
```

**Note:** Yanking doesn't delete; existing users can still use it.

---

## CI/CD Pipeline Details

### Workflow Triggers

The publish workflow (`publish-ovsm.yml`) triggers on:

1. **Git tags**: `ovsm-v*` (e.g., `ovsm-v1.0.1`)
2. **Manual**: Via GitHub Actions UI

### Workflow Steps

1. **Verify and Test**
   - Check formatting
   - Run clippy
   - Build release binary
   - Run all tests
   - Test all examples
   - Generate docs
   - Verify package

2. **Publish**
   - Extract version from tag
   - Verify version matches `Cargo.toml`
   - Publish to crates.io (or dry-run)
   - Create GitHub release

3. **Deploy Docs**
   - Generate documentation
   - Deploy to GitHub Pages

4. **Notify**
   - Print success message with links

---

## Maintenance

### Regular Updates

- **Dependencies**: Update monthly with `cargo update`
- **Rust toolchain**: Test with latest stable
- **Security**: Run `cargo audit` regularly
- **Documentation**: Keep examples and guides current

### Long-Term Support

- Maintain changelog
- Respond to issues promptly
- Tag releases appropriately
- Keep documentation accurate

---

## Additional Resources

- **Cargo Book**: https://doc.rust-lang.org/cargo/
- **Publishing Guide**: https://doc.rust-lang.org/cargo/reference/publishing.html
- **Semantic Versioning**: https://semver.org/
- **Crates.io Policies**: https://crates.io/policies

---

## Quick Reference

```bash
# Update version
vim crates/ovsm/Cargo.toml

# Update changelog
vim crates/ovsm/CHANGELOG.md

# Commit and tag
git add crates/ovsm/Cargo.toml crates/ovsm/CHANGELOG.md
git commit -m "chore(ovsm): bump version to X.Y.Z"
git push origin main
git tag ovsm-vX.Y.Z
git push origin ovsm-vX.Y.Z

# Manual publish (if needed)
cd crates/ovsm
cargo publish

# Verify
open https://crates.io/crates/ovsm
open https://docs.rs/ovsm
```

---

**Need help?** Open an issue or contact the maintainers.
