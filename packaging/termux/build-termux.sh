#!/bin/bash
# Termux package build script for OSVM CLI
# Usage: ./build-termux.sh [version] [architecture]

set -e

# Get version and architecture from command line or defaults
VERSION="${1:-$(git describe --tags --abbrev=0 | sed 's/^v//')}"
ARCH="${2:-aarch64}"
PACKAGE_NAME="osvm"

echo "Building Termux package for osvm version $VERSION ($ARCH)"

# Go to repository root
cd "$(dirname "$0")/../.."

# Build the binary first
echo "Building binary..."
cargo build --release

# Create package directory structure
PACKAGE_DIR="${PACKAGE_NAME}_${VERSION}_${ARCH}"
rm -rf "$PACKAGE_DIR"

mkdir -p "$PACKAGE_DIR"/data/data/com.termux/files/usr/bin
mkdir -p "$PACKAGE_DIR"/control

# Copy binary
cp target/release/osvm "$PACKAGE_DIR"/data/data/com.termux/files/usr/bin/

# Create control file
cat > "$PACKAGE_DIR"/control/control << EOF
Package: $PACKAGE_NAME
Version: $VERSION
Architecture: $ARCH
Maintainer: OpenSVM <rin@opensvm.com>
Description: OpenSVM CLI tool for managing SVM nodes and deployments
 OSVM CLI is a comprehensive tool for managing Solana Virtual Machine (SVM)
 nodes and deployments. It provides functionality for node deployment,
 SVM blockchain interactions, audit capabilities, and SSH-based deployment
 automation.
Homepage: https://github.com/openSVM/osvm-cli
Section: utils
Priority: optional
Depends: rust
Installed-Size: $(du -k "$PACKAGE_DIR"/data | tail -1 | cut -f1)
EOF

# Create prerm script to handle cleanup
cat > "$PACKAGE_DIR"/control/prerm << 'EOF'
#!/bin/sh
# Clean up any cached data
rm -rf "$HOME/.osvm" 2>/dev/null || true
exit 0
EOF
chmod +x "$PACKAGE_DIR"/control/prerm

# Create postinst script
cat > "$PACKAGE_DIR"/control/postinst << 'EOF'
#!/bin/sh
# Ensure binary is executable
chmod +x "$PREFIX/bin/osvm" 2>/dev/null || true
echo "OpenSVM CLI installed successfully!"
echo "Run 'osvm --help' to get started."
exit 0
EOF
chmod +x "$PACKAGE_DIR"/control/postinst

# Create postrm script
cat > "$PACKAGE_DIR"/control/postrm << 'EOF'
#!/bin/sh
# Clean up after removal
echo "OpenSVM CLI has been removed."
echo "Configuration files in ~/.osvm have been preserved."
exit 0
EOF
chmod +x "$PACKAGE_DIR"/control/postrm

# Create the .deb package for Termux
echo "Building package..."
dpkg-deb --build "$PACKAGE_DIR"

echo "Termux package built successfully!"
echo "Package file: ${PACKAGE_DIR}.deb"
echo ""
echo "To install on Termux:"
echo "  1. Transfer the .deb file to your Termux environment"
echo "  2. Run: dpkg -i ${PACKAGE_DIR}.deb"
echo "  3. Or use: apt install ./${PACKAGE_DIR}.deb"