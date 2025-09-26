#!/bin/bash
# Termux package build script for OSVM CLI
# Usage: ./build-termux.sh [version] [architecture]

set -e

# Source packaging configuration
SCRIPT_DIR="$(dirname "$0")"
source "$SCRIPT_DIR/../config.sh"

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
mkdir -p "$PACKAGE_DIR"/DEBIAN

# Copy binary
cp target/release/osvm "$PACKAGE_DIR"/data/data/com.termux/files/usr/bin/

# Create control file
cat > "$PACKAGE_DIR"/DEBIAN/control << EOF
Package: $PACKAGE_NAME
Version: $VERSION
Architecture: $ARCH
Maintainer: $OSVM_MAINTAINER_NAME <$OSVM_MAINTAINER_EMAIL>
Description: $OSVM_DESCRIPTION_SHORT
 $OSVM_DESCRIPTION_LONG
Homepage: $OSVM_PROJECT_URL
Section: utils
Priority: optional
Depends: rust
Installed-Size: $(du -k "$PACKAGE_DIR"/data | tail -1 | cut -f1)
EOF

# Create prerm script to handle cleanup
cat > "$PACKAGE_DIR"/DEBIAN/prerm << 'EOF'
#!/bin/sh
# Clean up any cached data
rm -rf "$HOME/.osvm" 2>/dev/null || true
exit 0
EOF
chmod +x "$PACKAGE_DIR"/DEBIAN/prerm

# Create postinst script
cat > "$PACKAGE_DIR"/DEBIAN/postinst << 'EOF'
#!/bin/sh
# Ensure binary is executable
chmod +x "/data/data/com.termux/files/usr/bin/osvm" 2>/dev/null || true
echo "OpenSVM CLI installed successfully!"
echo "Run 'osvm --help' to get started."
exit 0
EOF
chmod +x "$PACKAGE_DIR"/DEBIAN/postinst

# Create postrm script
cat > "$PACKAGE_DIR"/DEBIAN/postrm << 'EOF'
#!/bin/sh
# Clean up after removal
echo "OpenSVM CLI has been removed."
echo "Configuration files in ~/.osvm have been preserved."
exit 0
EOF
chmod +x "$PACKAGE_DIR"/DEBIAN/postrm

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
