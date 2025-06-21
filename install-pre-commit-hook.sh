#!/bin/bash
#
# Install pre-commit hook for osvm-cli
# This script copies the pre-commit hook to the .git/hooks directory
#

# Check if we're in the right directory
if [ ! -f "Cargo.toml" ] || [ ! -d ".git" ]; then
    echo "❌ Error: This script must be run from the root of the osvm-cli repository"
    exit 1
fi

# Check if pre-commit script exists
if [ ! -f "pre-commit" ]; then
    echo "❌ Error: pre-commit script not found in current directory"
    exit 1
fi

# Install the pre-commit hook
echo "🔧 Installing pre-commit hook..."
cp pre-commit .git/hooks/pre-commit
chmod +x .git/hooks/pre-commit

echo "✅ Pre-commit hook installed successfully!"
echo ""
echo "The hook will now run 'cargo fmt --all -- --check' and 'cargo clippy' before each commit."
echo ""
echo "To skip the pre-commit hook for a specific commit, use:"
echo "  git commit --no-verify -m 'your message'"
echo ""
echo "To uninstall the hook, run:"
echo "  rm .git/hooks/pre-commit"