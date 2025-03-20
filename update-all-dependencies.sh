#!/usr/bin/env bash
#
# Updates all dependencies in Cargo.toml to their latest versions
#

set -e

# Log file for tracking updates
LOG_FILE="dependency_update.log"
echo "Dependency update log - $(date)" > "$LOG_FILE"

# Trap to handle errors and cleanup
trap 'echo "An error occurred. Rolling back changes..."; git checkout -- Cargo.toml; exit 1' ERR

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
      echo "Found update for $package: $version -> $latest_version"
      read -p "Do you want to update $package to $latest_version? (y/n): " confirm
      if [[ "$confirm" == "y" ]]; then
        echo "Updating $package from $version to $latest_version"
        # Detect platform-specific sed in-place flag (GNU vs. BSD sed)
        if sed --version >/dev/null 2>&1; then
          SED_INPLACE=(-i)
        else
          SED_INPLACE=(-i '')
        fi

        if grep -q "$package = { version =" Cargo.toml; then
          sed "${SED_INPLACE[@]}" "s/$package = { version = \"$version\"/$package = { version = \"$latest_version\"/" Cargo.toml
        else
          sed "${SED_INPLACE[@]}" "s/$package = \"$version\"/$package = \"$latest_version\"/" Cargo.toml
        fi
        echo "$package updated from $version to $latest_version" >> "$LOG_FILE"
      else
        echo "Skipped updating $package" >> "$LOG_FILE"
      fi
      # Detect platform-specific sed in-place flag (GNU vs. BSD sed)
      if sed --version >/dev/null 2>&1; then
        SED_INPLACE=(-i)
      else
        SED_INPLACE=(-i '')
      fi

      if grep -q "$package = { version =" Cargo.toml; then
        sed "${SED_INPLACE[@]}" "s/$package = { version = \"$version\"/$package = { version = \"$latest_version\"/" Cargo.toml
      else
        sed "${SED_INPLACE[@]}" "s/$package = \"$version\"/$package = \"$latest_version\"/" Cargo.toml
      fi
      if [[ "$confirm" == "y" ]]; then
        echo "Updating $package from $version to $latest_version"
        # Use sed to update the version in Cargo.toml
        # Handle both simple dependencies and those with features
        if command -v cargo-add &> /dev/null; then
          cargo add "$package"@"$latest_version" --quiet
        else
          echo "cargo-add not found. Please install cargo-edit to safely update dependencies."
          exit 1
        fi
        echo "$package updated from $version to $latest_version" >> "$LOG_FILE"
      else
        echo "Skipped updating $package" >> "$LOG_FILE"
      fi
        sed -i "s/$package = { version = \"$version\"/$package = { version = \"$latest_version\"/" Cargo.toml
      else
        sed -i "s/$package = \"$version\"/$package = \"$latest_version\"/" Cargo.toml
      fi
    else
      echo "Could not find latest version for $package, skipping..."
    fi
        echo "Found update for $package: $version -> $latest_version"
        read -p "Do you want to update $package to $latest_version? (y/n): " confirm
        if [[ "$confirm" == "y" ]]; then
          echo "Proposed update for $package: $version -> $latest_version" >> "$LOG_FILE"
          echo "Please review and test the changes manually before applying them."
        else
          echo "Skipped updating $package" >> "$LOG_FILE"
        fi
  # Update dependencies with features
  echo "Updating dependencies with features..."
  echo "Parsing Cargo.toml using a TOML parser..."
  if command -v cargo-upgrade &> /dev/null; then
    echo "Using cargo-upgrade to update dependencies with features..."
    cargo upgrade --all
  else
    echo "cargo-upgrade not found. Please install cargo-edit to safely update dependencies."
    exit 1
  fi
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