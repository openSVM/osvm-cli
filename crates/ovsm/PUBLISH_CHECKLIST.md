# OVSM Publishing Checklist

Quick reference for publishing new versions of the OVSM crate.

## Pre-Release Checklist

### Code Quality
- [ ] All tests pass: `cargo test`
- [ ] Formatting clean: `cargo fmt --all -- --check`
- [ ] Clippy clean: `cargo clippy --all-targets --all-features`
- [ ] Examples work: Test all `.ovsm` scripts
- [ ] Documentation builds: `cargo doc --no-deps`
- [ ] Package builds: `cargo package --allow-dirty`

### Documentation
- [ ] README.md updated
- [ ] CHANGELOG.md updated with new version
- [ ] Version number incremented in Cargo.toml
- [ ] Usage guides reflect new features
- [ ] API docs complete and accurate

### Files
- [ ] No sensitive data in files
- [ ] License file present
- [ ] All necessary files included
- [ ] Unnecessary files excluded

## Release Steps

### 1. Update Version

```bash
# Edit Cargo.toml
vim crates/ovsm/Cargo.toml
# Change: version = "1.0.1"
```

### 2. Update Changelog

```bash
# Edit CHANGELOG.md
vim crates/ovsm/CHANGELOG.md
# Add section for new version
```

### 3. Test Package

```bash
cd crates/ovsm
cargo package --allow-dirty
cargo package --allow-dirty --list | less
```

### 4. Commit Changes

```bash
git add crates/ovsm/Cargo.toml crates/ovsm/CHANGELOG.md
git commit -m "chore(ovsm): bump version to X.Y.Z"
git push origin main
```

### 5. Create Tag

```bash
git tag ovsm-vX.Y.Z -m "OVSM vX.Y.Z"
git push origin ovsm-vX.Y.Z
```

### 6. Monitor Workflow

- Go to: https://github.com/opensvm/osvm-cli/actions
- Watch: "Publish OVSM Crate" workflow
- Verify: All jobs pass (green checkmarks)

### 7. Verify Publication

- [ ] Check crates.io: https://crates.io/crates/ovsm
- [ ] Check docs.rs: https://docs.rs/ovsm
- [ ] Check GitHub Release: https://github.com/opensvm/osvm-cli/releases
- [ ] Test install: `cargo install ovsm --version X.Y.Z`

## Post-Release Checklist

### Immediate
- [ ] Verify package on crates.io
- [ ] Verify docs on docs.rs
- [ ] Test installation works
- [ ] GitHub Release created with notes

### Within 24 Hours
- [ ] Update main repository README if needed
- [ ] Announce on social media / community channels
- [ ] Monitor for issues or bug reports
- [ ] Respond to questions

### Within 1 Week
- [ ] Review download statistics
- [ ] Address any reported issues
- [ ] Plan next release if needed

## Quick Commands

```bash
# Current version
grep "^version" crates/ovsm/Cargo.toml

# Test everything
cd crates/ovsm && cargo test && cargo run --example run_file examples/hello_world.ovsm

# Package dry run
cargo package --allow-dirty --list

# Create and push tag
git tag ovsm-v1.0.1 -m "OVSM v1.0.1" && git push origin ovsm-v1.0.1

# Verify on crates.io (after publish)
cargo search ovsm

# Install from crates.io (after publish)
cargo install ovsm
```

## Version Numbering Guide

| Change Type | Example | When to Use |
|-------------|---------|-------------|
| MAJOR | 1.0.0 → 2.0.0 | Breaking API changes |
| MINOR | 1.0.0 → 1.1.0 | New features, backward compatible |
| PATCH | 1.0.0 → 1.0.1 | Bug fixes, backward compatible |

## Emergency Rollback

If you need to yank a broken release:

```bash
# Yank version from crates.io
cargo yank --vers X.Y.Z

# Create hotfix
# 1. Fix the issue
# 2. Increment patch version
# 3. Follow normal release process

# Unyank if needed
cargo yank --vers X.Y.Z --undo
```

## Common Issues

| Issue | Solution |
|-------|----------|
| Version already exists | Increment version number |
| Tests fail in CI | Check logs, fix tests, re-tag |
| Auth failed | Verify CARGO_REGISTRY_TOKEN secret |
| Tag exists | Delete and recreate with new name |
| Package too large | Add more items to exclude list |

## Files to Update

```
crates/ovsm/
├── Cargo.toml          # Version number
├── CHANGELOG.md        # Release notes
├── README.md           # If needed
├── USAGE_GUIDE.md      # If features added
└── HOW_TO_USE.md       # If workflow changed
```

## Automation

The GitHub Actions workflow automatically:
- ✅ Runs all tests
- ✅ Checks formatting and linting
- ✅ Tests all examples
- ✅ Publishes to crates.io
- ✅ Creates GitHub Release
- ✅ Deploys documentation
- ✅ Notifies on success

## Manual Override

To publish manually (not recommended):

```bash
cd crates/ovsm
export CARGO_REGISTRY_TOKEN="your-token"
cargo publish
```

## Support

- Documentation: See PUBLISHING.md
- Issues: https://github.com/opensvm/osvm-cli/issues
- Actions: https://github.com/opensvm/osvm-cli/actions

---

**Remember:** Once published, versions cannot be changed. Always test thoroughly!
