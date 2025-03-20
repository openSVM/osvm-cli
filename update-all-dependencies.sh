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
  cargo upgrade --all
else
  echo "cargo-upgrade not found, using manual update method..."
  
  # Update regular dependencies
  echo "Updating regular dependencies..."
  while read -r package version; do
    echo "Checking $package (current: $version)..."
    latest_version=$(get_latest_version "$package")
    if [ -n "$latest_version" ]; then
      echo "Updating $package from $version to $latest_version"
      # Use sed to update the version in Cargo.toml
      # Handle both simple dependencies and those with features
      if grep -q "$package = { version =" Cargo.toml; then
        sed -i "s/$package = { version = \"$version\"/$package = { version = \"$latest_version\"/" Cargo.toml
      else
        sed -i "s/$package = \"$version\"/$package = \"$latest_version\"/" Cargo.toml
      fi
    else
      echo "Could not find latest version for $package, skipping..."
    fi
  done < <(grep -E '^[a-zA-Z0-9_-]+ *= *"[^"]+"' Cargo.toml | grep -v '^\[' | sed -E 's/([a-zA-Z0-9_-]+)[[:space:]]*=[[:space:]]*"([^"]+)".*/\1 \2/')
  
  # Update dependencies with features
  echo "Updating dependencies with features..."
  while read -r line; do
    if [[ $line =~ ^([a-zA-Z0-9_-]+)[[:space:]]*=[[:space:]]*\{.*version[[:space:]]*=[[:space:]]*\"([^\"]+)\" ]]; then
      package="${BASH_REMATCH[1]}"
      version="${BASH_REMATCH[2]}"
      echo "Checking $package (current: $version)..."
      latest_version=$(get_latest_version "$package")
      if [ -n "$latest_version" ]; then
        echo "Updating $package from $version to $latest_version"
        sed -i "s/version = \"$version\"/version = \"$latest_version\"/" Cargo.toml
      else
        echo "Could not find latest version for $package, skipping..."
      fi
    fi
  done < <(grep -E '^[a-zA-Z0-9_-]+ *= *\{.*version *=' Cargo.toml)
fi

# Special handling for Solana dependencies
echo "Updating Solana dependencies..."
latest_solana_version=$(get_latest_version "solana-sdk")
if [ -n "$latest_solana_version" ]; then
  echo "Using Solana version $latest_solana_version"
  ./update-solana-dependencies.sh "$latest_solana_version"
fi

echo "All dependencies updated to latest versions in Cargo.toml"
echo "Running cargo update to update Cargo.lock..."
cargo update

echo "Done!"