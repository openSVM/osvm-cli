#!/usr/bin/env bash
#
# Updates specific transitive dependencies to the versions we want
#

set -e

echo "Updating transitive dependencies..."

# Add the dependencies to Cargo.toml
echo "Adding dependencies to Cargo.toml..."
cat >> Cargo.toml << EOF

# Temporary dependencies to force specific versions of transitive dependencies
# These will be removed after cargo update
curve25519-dalek = "3.2.1"
solana-feature-set = "2.2.4"
EOF

# Update the dependencies
echo "Running cargo update..."
cargo update

# Remove the temporary dependencies from Cargo.toml
echo "Removing temporary dependencies from Cargo.toml..."
sed -i '/# Temporary dependencies/,/solana-feature-set/d' Cargo.toml

echo "Done!"