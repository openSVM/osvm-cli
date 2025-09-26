#!/bin/bash
# ArchLinux package build script for OSVM CLI
# Usage: ./build-arch.sh [version]

set -e

# Get version from command line or git tag
VERSION="${1:-$(git describe --tags --abbrev=0 | sed 's/^v//')}"

echo "Building ArchLinux package for osvm version $VERSION"

# Go to repository root
cd "$(dirname "$0")/../.."

# Create source tarball
SOURCE_NAME="osvm-${VERSION}.tar.gz"
git archive --format=tar.gz --prefix="osvm-cli-${VERSION}/" HEAD > "$SOURCE_NAME"

# Calculate SHA256
SHA256=$(sha256sum "$SOURCE_NAME" | cut -d ' ' -f 1)

# Update PKGBUILD with current version and SHA
sed -i "s/pkgver=.*/pkgver=${VERSION}/" packaging/archlinux/PKGBUILD
sed -i "s/sha256sums=.*/sha256sums=('${SHA256}')/" packaging/archlinux/PKGBUILD

echo "Updated PKGBUILD:"
echo "  Version: $VERSION"
echo "  SHA256: $SHA256"

# Generate .SRCINFO file for AUR
cd packaging/archlinux
makepkg --printsrcinfo > .SRCINFO

echo "ArchLinux package files prepared successfully!"
echo "Files generated:"
echo "  - PKGBUILD"
echo "  - .SRCINFO"
echo ""
echo "To build the package locally, run:"
echo "  cd packaging/archlinux && makepkg -sri"
echo ""
echo "To submit to AUR:"
echo "  1. Clone AUR repo: git clone ssh://aur@aur.archlinux.org/osvm.git"
echo "  2. Copy files: cp PKGBUILD .SRCINFO osvm/"
echo "  3. Commit and push: cd osvm && git add . && git commit -m 'Update to $VERSION' && git push"