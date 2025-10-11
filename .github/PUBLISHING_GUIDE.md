# GitHub Actions Publishing Guide for OVSM

This guide explains the CI/CD setup for automatically publishing the OVSM crate to crates.io.

## Overview

The repository now has automated publishing configured for the OVSM crate through GitHub Actions.

## Workflows

### 1. CI Workflow (`.github/workflows/ci.yml`)

**Triggers:** Push to `main`, Pull Requests

**Jobs:**
- ✅ **Sanity Checks**: Formatting, clippy
- ✅ **Unit Tests**: Main CLI tests
- ✅ **E2E Tests**: Integration tests
- ✅ **OVSM Tests**: OVSM crate tests (NEW)
  - Build OVSM crate
  - Run unit tests
  - Run integration tests
  - Test all example scripts
- ✅ **Code Coverage**: Coverage reports for both main CLI and OVSM crate

### 2. Publish OVSM Workflow (`.github/workflows/publish-ovsm.yml`) - NEW

**Triggers:**
- Git tags matching `ovsm-v*` (e.g., `ovsm-v1.0.0`)
- Manual trigger via GitHub Actions UI

**Jobs:**

#### Job 1: Verify and Test
- Check code formatting
- Run clippy linting
- Build release binary
- Run all tests
- Test all example scripts
- Generate documentation
- Verify package builds

#### Job 2: Publish
- Extract version from git tag
- Verify version matches `Cargo.toml`
- Publish to crates.io
- Create GitHub Release with:
  - Release notes
  - Installation instructions
  - Documentation links
  - Example links

#### Job 3: Deploy Docs
- Generate API documentation
- Deploy to GitHub Pages

#### Job 4: Notify
- Print success message with links

## Setup Requirements

### 1. Crates.io Token

You need to add your crates.io API token as a GitHub secret:

1. Go to https://crates.io/me
2. Generate API token
3. Go to repository Settings → Secrets and variables → Actions
4. Create new repository secret:
   - Name: `CARGO_REGISTRY_TOKEN`
   - Value: Your crates.io token

### 2. Repository Permissions

Ensure GitHub Actions has write permissions:

1. Go to Settings → Actions → General
2. Under "Workflow permissions", select "Read and write permissions"
3. Check "Allow GitHub Actions to create and approve pull requests"

## Publishing Process

### Automatic Publishing (Recommended)

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

### Fixed
- Bug fix Y
```

#### Step 3: Commit Changes

```bash
cd /home/larp/larpdevs/osvm-cli
git add crates/ovsm/Cargo.toml crates/ovsm/CHANGELOG.md
git commit -m "chore(ovsm): bump version to 1.0.1"
git push origin main
```

#### Step 4: Create and Push Tag

```bash
git tag ovsm-v1.0.1 -m "OVSM v1.0.1"
git push origin ovsm-v1.0.1
```

#### Step 5: Monitor Workflow

1. Go to Actions tab in GitHub
2. Watch "Publish OVSM Crate" workflow
3. Verify all jobs pass
4. Check crates.io and docs.rs

### Manual Publishing

Alternatively, you can manually trigger the workflow:

1. Go to Actions → Publish OVSM Crate
2. Click "Run workflow"
3. Select branch (usually `main`)
4. Set "Dry run" to `true` for testing (optional)
5. Click "Run workflow"

### Dry Run (Testing)

Test the publishing workflow without actually publishing:

```bash
# Via GitHub Actions UI: Set dry_run = true
```

Or locally:

```bash
cd crates/ovsm
cargo publish --dry-run --allow-dirty
```

## Verification Checklist

After publishing, verify:

- [ ] Package appears on crates.io: https://crates.io/crates/ovsm
- [ ] Documentation builds on docs.rs: https://docs.rs/ovsm
- [ ] GitHub Release created with proper notes
- [ ] Version badge updates in README
- [ ] Package can be installed: `cargo install ovsm`
- [ ] Examples work from published crate

## Workflow Files

### `.github/workflows/publish-ovsm.yml`

```yaml
name: Publish OVSM Crate

on:
  push:
    tags:
      - 'ovsm-v*'
  workflow_dispatch:
    inputs:
      dry_run:
        description: 'Dry run (don't actually publish)'
        required: false
        default: 'false'

# ... (see file for full details)
```

### `.github/workflows/ci.yml`

Updated to include OVSM crate testing:

```yaml
ovsm-tests:
  name: OVSM Crate Tests
  needs: sanity-check
  runs-on: ubuntu-latest
  steps:
    - Build OVSM crate
    - Run unit tests
    - Run integration tests
    - Test example scripts
```

## Troubleshooting

### Error: Version Already Published

**Problem:** `error: crate version X.Y.Z is already uploaded`

**Solution:** You cannot re-publish the same version. Increment version in `Cargo.toml`:

```toml
version = "1.0.2"  # Increment
```

### Error: Authentication Failed

**Problem:** `error: authentication failed`

**Solution:**
1. Verify `CARGO_REGISTRY_TOKEN` secret is set correctly
2. Check token hasn't expired
3. Generate new token if needed

### Error: Tag Already Exists

**Problem:** `error: tag 'ovsm-v1.0.0' already exists`

**Solution:**

```bash
# Delete local tag
git tag -d ovsm-v1.0.0

# Delete remote tag
git push origin :refs/tags/ovsm-v1.0.0

# Create new tag
git tag ovsm-v1.0.1
git push origin ovsm-v1.0.1
```

### Workflow Fails on Tests

**Problem:** Tests fail in CI but pass locally

**Solution:**
1. Check GitHub Actions logs for specific failures
2. Verify all dependencies are properly specified
3. Check if examples need specific files
4. Run `cargo package --list` to verify all files are included

## Tag Naming Convention

OVSM releases use a separate tag namespace:

- **OVSM crate**: `ovsm-v1.0.0`, `ovsm-v1.0.1`, etc.
- **Main CLI**: `v1.0.0`, `v1.1.0`, etc.

This allows independent versioning of the OVSM interpreter and the main CLI tool.

## Version Numbering

Follow [Semantic Versioning](https://semver.org/):

- **MAJOR** (X.0.0): Breaking API changes
- **MINOR** (1.X.0): New features, backward compatible
- **PATCH** (1.0.X): Bug fixes, backward compatible

## Release Schedule

Recommended release cadence:

- **Patch releases**: As needed for bug fixes
- **Minor releases**: Monthly or when significant features are added
- **Major releases**: Quarterly or when breaking changes are necessary

## Post-Release Tasks

After successful publication:

1. **Verify Installation**
   ```bash
   cargo install ovsm --version 1.0.1
   ovsm --help  # If CLI installed
   ```

2. **Update Main README**
   Update version badges and installation instructions

3. **Announce Release**
   - GitHub Discussions
   - Social media
   - Community channels

4. **Monitor Issues**
   - Watch for bug reports
   - Respond to questions
   - Update documentation as needed

## Security Considerations

- ✅ Never commit API tokens to repository
- ✅ Use GitHub Secrets for sensitive data
- ✅ Regularly rotate API tokens
- ✅ Review workflow logs for sensitive data leaks
- ✅ Limit workflow permissions to minimum required

## Additional Resources

- [Cargo Publishing Guide](https://doc.rust-lang.org/cargo/reference/publishing.html)
- [GitHub Actions Docs](https://docs.github.com/en/actions)
- [Crates.io Policies](https://crates.io/policies)
- [Semantic Versioning](https://semver.org/)

## Support

For issues or questions:
- GitHub Issues: https://github.com/opensvm/osvm-cli/issues
- Discussions: https://github.com/opensvm/osvm-cli/discussions

---

**Last Updated:** 2025-10-11
**Maintained By:** OSVM Team
