#!/bin/bash
# Debian package build script for OSVM CLI
# Usage: ./build-deb.sh [version]

set -e

# Get version from command line or git tag
VERSION="${1:-$(git describe --tags --abbrev=0 | sed 's/^v//')}"
WORKDIR=$(pwd)
PKG="osvm-$VERSION"
TARBALL="osvm_${VERSION}.orig.tar.gz"
TEMP_TARBALL="temp_${TARBALL}"

echo "Building Debian package for osvm version $VERSION"

# Clean up any old stuff
rm -rf "$PKG"
rm -f "$TARBALL" "$TEMP_TARBALL"

# Go to repository root
cd "$(dirname "$0")/../.."

# Prepare source directory
mkdir -p "$PKG"
cp -r src Cargo.toml Cargo.lock README.md LICENSE "$PKG/"

# Pack source tarball with temporary name first
tar czvf "$TEMP_TARBALL" "$PKG"

# Ensure the tarball exists
if [ ! -f "$TEMP_TARBALL" ]; then
    echo "Error: $TEMP_TARBALL not found!"
    exit 1
fi

# Move into the package directory
cd "$PKG"

# Set environment variables for dh_make to use correct maintainer info
export DEBFULLNAME="OpenSVM"
export DEBEMAIL="rin@opensvm.com"

# Run dh_make using the temporary tarball - this avoids same-file error
# dh_make will copy the temp tarball to the correct orig.tar.gz location
dh_make -y -s -c apache -e rin@opensvm.com -f "../$TEMP_TARBALL"

# Fix maintainer name in generated files (dh_make sometimes ignores DEBFULLNAME)
sed -i 's/Maintainer: unknown/Maintainer: OpenSVM/' debian/control
sed -i 's/ -- unknown/ -- OpenSVM/' debian/changelog

# Customize the debian/control file for better dependencies
cat > debian/control << 'EOF'
Source: osvm
Section: utils
Priority: optional
Maintainer: OpenSVM <rin@opensvm.com>
Build-Depends: debhelper (>= 10), cargo, rustc, pkg-config, libssl-dev, libudev-dev, libusb-1.0-0-dev
Standards-Version: 4.1.3
Homepage: https://github.com/openSVM/osvm-cli

Package: osvm
Architecture: any
Depends: ${shlibs:Depends}, ${misc:Depends}
Description: OpenSVM CLI tool for managing SVM nodes and deployments
 OSVM CLI is a comprehensive tool for managing Solana Virtual Machine (SVM)
 nodes and deployments. It provides functionality for:
 - Node deployment and management
 - SVM blockchain interactions
 - Audit capabilities
 - SSH-based deployment automation
 .
 This tool is designed to simplify the process of working with SVM-based
 blockchain networks including Solana, Sonic, and other compatible chains.
EOF

# Customize debian/rules for Cargo-based builds
cat > debian/rules << 'EOF'
#!/usr/bin/make -f
# See debhelper(7) (uncomment to enable)
# output every command that modifies files on the build system.
#export DH_VERBOSE = 1

%:
	dh $@

override_dh_auto_build:
	cargo build --release

override_dh_auto_install:
	install -D -m755 target/release/osvm debian/osvm/usr/bin/osvm

override_dh_auto_test:
	# Skip tests for packaging to avoid network dependencies
	true

override_dh_auto_clean:
	cargo clean || true
	dh_auto_clean
EOF

# Make rules executable
chmod +x debian/rules

# Build the .deb package from within the package directory where debian/ folder was created
echo "Building package..."
dpkg-buildpackage -us -uc

# Move back to the working directory
cd "$WORKDIR"

# Clean up temporary tarball
rm -f "$TEMP_TARBALL"

echo "Debian package built successfully!"
echo "Package files are in the current directory:"
ls -la *.deb 2>/dev/null || echo "No .deb files found in current directory, check parent directory"