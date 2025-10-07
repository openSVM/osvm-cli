#!/bin/bash
# Simplified Docker build script with multiple approaches

set -e

echo "🐳 Building osvm-cli Docker image..."
echo ""

# Approach 1: Try with the network-fixed Dockerfile
echo "Method 1: Building with network-resilient Dockerfile..."
if docker build --network=host -f Dockerfile.network-fix -t osvm-cli:latest . 2>/dev/null; then
    echo "✅ Build successful!"
    exit 0
fi

# Approach 2: Try original Dockerfile with explicit DNS
echo "Method 2: Building with DNS arguments..."
if docker build \
    --network=host \
    --build-arg http_proxy="${http_proxy}" \
    --build-arg https_proxy="${https_proxy}" \
    --build-arg HTTP_PROXY="${HTTP_PROXY}" \
    --build-arg HTTPS_PROXY="${HTTPS_PROXY}" \
    -f Dockerfile \
    -t osvm-cli:latest . 2>/dev/null; then
    echo "✅ Build successful!"
    exit 0
fi

# Approach 3: Try with Docker BuildKit
echo "Method 3: Building with BuildKit..."
if DOCKER_BUILDKIT=1 docker build \
    --network=host \
    --progress=plain \
    -f Dockerfile.network-fix \
    -t osvm-cli:latest . 2>/dev/null; then
    echo "✅ Build successful!"
    exit 0
fi

# If all methods fail, provide detailed error output
echo ""
echo "❌ Quick build attempts failed. Running with verbose output for debugging..."
echo ""

docker build --network=host -f Dockerfile.network-fix -t osvm-cli:latest .

if [ $? -ne 0 ]; then
    echo ""
    echo "🔧 Troubleshooting suggestions:"
    echo ""
    echo "1. Check Docker daemon status:"
    echo "   sudo systemctl status docker"
    echo ""
    echo "2. Check network connectivity:"
    echo "   curl -I https://deb.debian.org"
    echo ""
    echo "3. If behind a proxy, set environment variables:"
    echo "   export http_proxy=http://your-proxy:port"
    echo "   export https_proxy=http://your-proxy:port"
    echo ""
    echo "4. Try restarting Docker:"
    echo "   sudo systemctl restart docker"
    echo ""
    echo "5. Check Docker DNS configuration:"
    echo "   docker run --rm alpine nslookup deb.debian.org"
    echo ""
    exit 1
fi

echo "✅ Docker image built successfully!"
echo ""
echo "To run the container:"
echo "  docker run --rm osvm-cli:latest --help"
echo ""
echo "To run with your local config:"
echo "  docker run --rm -v ~/.config/solana:/home/osvm/.config/solana:ro osvm-cli:latest balance"