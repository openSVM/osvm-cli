#!/usr/bin/env bash
# Docker build script for osvm-cli that works with Tailscale VPN
#
# This script uses --network=host to allow Docker builds to use
# the host's network stack, which includes Tailscale DNS resolution.

set -e

echo "🐳 Building osvm-cli Docker image..."
echo "📡 Using host network (Tailscale-compatible)"

docker build \
  --network=host \
  -t osvm-cli \
  "$@" \
  .

echo "✅ Build complete!"
echo ""
echo "Run with: docker run --rm osvm-cli --help"
echo "Or: docker run --rm osvm-cli --version"
