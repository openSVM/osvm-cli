#!/bin/bash
# Helper script to run OSVM CLI in Docker on macOS
# Usage: ./scripts/docker-run-macos.sh [osvm arguments]

# Build the image if it doesn't exist
if [[ "$(docker images -q osvm-cli:latest 2> /dev/null)" == "" ]]; then
  echo "Building OSVM Docker image..."
  docker build -f scripts/docker/Dockerfile-macos -t osvm-cli:latest .
fi

# Run OSVM in Docker with volume mounts for configuration
# Use -it only if we have a TTY
if [ -t 0 ]; then
  docker run --rm -it \
    -v ~/.config/solana:/home/osvm/.config/solana:ro \
    -v ~/.osvm:/home/osvm/.osvm \
    osvm-cli:latest "$@"
else
  docker run --rm \
    -v ~/.config/solana:/home/osvm/.config/solana:ro \
    -v ~/.osvm:/home/osvm/.osvm \
    osvm-cli:latest "$@"
fi

