#!/usr/bin/env bash
#
# Updates all dependencies in Cargo.toml to their latest versions
#

set -e

echo "Updating all dependencies to their latest versions..."

# Function to extract the latest version of a package from cargo search
get_latest_version() {
  local package=$1
  local latest_version=$(cargo search "^$package$" --limit 1 | head -n 1 | sed -E 's/.*= "([^"]+)".*/\1/')
  echo "$latest_version"
}

# Update dependencies using cargo-edit if available
if command -v cargo-upgrade &> /dev/null; then
  echo "Using cargo-upgrade to update dependencies..."
  cargo upgrade
  echo "Updating Solana dependencies to consistent version..."
  
  # Get the latest Solana SDK version
  LATEST_SOLANA_VERSION=$(get_latest_version "solana-sdk")
  echo "Latest Solana SDK version: $LATEST_SOLANA_VERSION"
  
  # Update all Solana dependencies to the same version
  cargo upgrade solana-clap-utils@$LATEST_SOLANA_VERSION
  cargo upgrade solana-cli-config@$LATEST_SOLANA_VERSION
  cargo upgrade solana-client@$LATEST_SOLANA_VERSION
  cargo upgrade solana-logger@$LATEST_SOLANA_VERSION
  cargo upgrade solana-remote-wallet@$LATEST_SOLANA_VERSION
  cargo upgrade solana-sdk@$LATEST_SOLANA_VERSION
else
  echo "cargo-upgrade not found, please install it with 'cargo install cargo-edit'"
  exit 1
fi

# Add patch entries for transitive dependencies that might cause issues
echo "Adding patch entries for transitive dependencies..."

# Check if patch entries already exist
if ! grep -q "\[patch.crates-io\]" Cargo.toml; then
  echo "
[patch.crates-io]
curve25519-dalek = { version = \"=3.2.1\" }
solana-feature-set = { version = \"$LATEST_SOLANA_VERSION\" }" >> Cargo.toml
fi

echo "Running cargo update to update Cargo.lock..."
cargo update

echo "Done! Dependencies updated to latest versions."