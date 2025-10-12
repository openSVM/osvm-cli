#!/usr/bin/env bash
#
# Updates specific transitive dependencies to the versions we want
#

set -e

echo "Updating transitive dependencies..."

# Update curve25519-dalek to version 3.2.1
echo "Updating curve25519-dalek to version 3.2.1..."
cargo update curve25519-dalek@3.2.0 --precise 3.2.1 || echo "Failed to update curve25519-dalek, it may already be at the desired version or there may be a dependency conflict."

# Update solana-feature-set to version 2.2.4
echo "Updating solana-feature-set to version 2.2.4..."
cargo update solana-feature-set --precise 2.2.4 || echo "Failed to update solana-feature-set, it may already be at the desired version or there may be a dependency conflict."

echo "Done!"