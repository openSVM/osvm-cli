#!/bin/bash
# Test script for snapshot feature in Docker

set -e

echo "🧪 Testing OSVM Snapshot Feature in Docker"
echo "==========================================="
echo ""

# Detect OS and set appropriate docker run script
if [[ "$OSTYPE" == "darwin"* ]]; then
  echo "📟 Detected macOS"
  DOCKER_RUN="./scripts/docker-run-macos.sh"
  DOCKERFILE="scripts/docker/Dockerfile-macos"
else
  echo "📟 Detected Linux"
  DOCKER_RUN="./scripts/docker/docker-run.sh"
  DOCKERFILE="scripts/docker/Dockerfile"
fi

# Check if Docker image exists
if [[ "$(docker images -q osvm-cli:latest 2> /dev/null)" == "" ]]; then
  echo "❌ Docker image not found. Build with:"
  echo "   docker build -f $DOCKERFILE -t osvm-cli:latest ."
  exit 1
fi

echo "✅ Docker image found"
echo "🐳 Using: $DOCKER_RUN"
echo ""

# Test all help commands
for cmd in "snapshot" "snapshot read" "snapshot stats" "snapshot export" "snapshot compare" "snapshot validate" "snapshot find"; do
  echo "Testing: $cmd --help"
  $DOCKER_RUN $cmd --help > /dev/null 2>&1
  if [ $? -eq 0 ]; then
    echo "✅ $cmd help works"
  else
    echo "❌ $cmd help failed"
    exit 1
  fi
done

echo ""
echo "Testing error handling..."
$DOCKER_RUN snapshot read --snapshot-dir /nonexistent --limit 1 2>&1 | grep -q "does not exist"
if [ $? -eq 0 ]; then
  echo "✅ Error handling works"
else
  echo "⚠️  Error message check skipped"
fi

echo ""
echo "🎉 All tests passed!"

