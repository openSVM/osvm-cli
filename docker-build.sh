#!/bin/bash
# Docker build script with network troubleshooting

set -e

echo "🔧 Building osvm-cli Docker image with network fixes..."

# Function to test DNS resolution
test_dns() {
    echo "Testing DNS resolution..."
    if host google.com > /dev/null 2>&1; then
        echo "✅ DNS is working"
        return 0
    else
        echo "⚠️  DNS resolution issues detected"
        return 1
    fi
}

# Function to build with standard Docker
build_standard() {
    echo "Building with standard Docker..."
    docker build \
        --network=host \
        --add-host=deb.debian.org:151.101.84.204 \
        --add-host=security.debian.org:151.101.84.204 \
        --add-host=cdn-fastly.deb.debian.org:151.101.84.204 \
        -f Dockerfile.fixed \
        -t osvm-cli:latest \
        .
}

# Function to build with Docker Compose
build_compose() {
    echo "Building with Docker Compose..."
    docker-compose build
}

# Function to build with BuildKit
build_buildkit() {
    echo "Building with Docker BuildKit..."
    DOCKER_BUILDKIT=1 docker build \
        --network=host \
        --progress=plain \
        -f Dockerfile.fixed \
        -t osvm-cli:latest \
        .
}

# Test DNS
if ! test_dns; then
    echo "⚠️  DNS issues detected. Adding workarounds..."

    # Try to use systemd-resolved if available
    if [ -f /run/systemd/resolve/resolv.conf ]; then
        echo "Using systemd-resolved configuration..."
        export DOCKER_OPTS="--dns 127.0.0.53"
    fi
fi

# Try different build methods
echo "Attempting build with standard Docker..."
if build_standard; then
    echo "✅ Build successful with standard Docker!"
    exit 0
fi

echo "Standard build failed, trying BuildKit..."
if build_buildkit; then
    echo "✅ Build successful with BuildKit!"
    exit 0
fi

echo "BuildKit failed, trying Docker Compose..."
if build_compose; then
    echo "✅ Build successful with Docker Compose!"
    exit 0
fi

echo "❌ All build methods failed. Please check your network configuration."
echo ""
echo "Troubleshooting steps:"
echo "1. Check Docker daemon DNS: docker info | grep -i dns"
echo "2. Restart Docker: sudo systemctl restart docker"
echo "3. Check firewall rules: sudo iptables -L"
echo "4. Try with a different network: --network=bridge or --network=none"
echo "5. Use a proxy if behind corporate firewall:"
echo "   docker build --build-arg http_proxy=\$http_proxy --build-arg https_proxy=\$https_proxy ."

exit 1