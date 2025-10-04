#!/bin/bash
set -e

# Build Alpine Linux rootfs with OSVM for Firecracker microVM
# This script creates a complete root filesystem image

ROOTFS_DIR="$HOME/.osvm/rootfs-build"
ROOTFS_IMG="$HOME/.osvm/rootfs/osvm-runtime.ext4"
ALPINE_VERSION="3.19"
ALPINE_MIRROR="https://dl-cdn.alpinelinux.org/alpine"

echo "🏗️  Building OSVM microVM rootfs..."

# Create directories
mkdir -p "$HOME/.osvm/rootfs"
rm -rf "$ROOTFS_DIR"
mkdir -p "$ROOTFS_DIR"

# Download Alpine minirootfs
echo "📦 Downloading Alpine Linux minirootfs..."
cd "$ROOTFS_DIR"
wget -q "${ALPINE_MIRROR}/v${ALPINE_VERSION}/releases/x86_64/alpine-minirootfs-${ALPINE_VERSION}.0-x86_64.tar.gz" \
    -O alpine-minirootfs.tar.gz

# Extract Alpine
echo "📂 Extracting Alpine Linux..."
tar xzf alpine-minirootfs.tar.gz
rm alpine-minirootfs.tar.gz

# Set up Alpine repositories
cat > "$ROOTFS_DIR/etc/apk/repositories" << EOF
${ALPINE_MIRROR}/v${ALPINE_VERSION}/main
${ALPINE_MIRROR}/v${ALPINE_VERSION}/community
EOF

# Copy OSVM binary
echo "📋 Copying OSVM binary..."
mkdir -p "$ROOTFS_DIR/usr/local/bin"
OSVM_SOURCE="/home/larp/larpdevs/osvm-cli/target/release/osvm"
cp "$OSVM_SOURCE" "$ROOTFS_DIR/usr/local/bin/"
chmod +x "$ROOTFS_DIR/usr/local/bin/osvm"

# Get OSVM dependencies using ldd
echo "📚 Identifying OSVM dependencies..."
OSVM_BIN="$OSVM_SOURCE"

# Create lib directories
mkdir -p "$ROOTFS_DIR/lib64"
mkdir -p "$ROOTFS_DIR/lib"
mkdir -p "$ROOTFS_DIR/usr/lib"

# Copy shared libraries
echo "📦 Copying shared libraries..."
for lib in $(ldd "$OSVM_BIN" | grep "=>" | awk '{print $3}'); do
    if [ -f "$lib" ]; then
        cp -L "$lib" "$ROOTFS_DIR/lib/" 2>/dev/null || true
        echo "  Copied: $lib"
    fi
done

# Copy dynamic linker
LINKER=$(ldd "$OSVM_BIN" | grep 'ld-linux' | awk '{print $1}')
if [ -f "$LINKER" ]; then
    cp -L "$LINKER" "$ROOTFS_DIR/lib64/" 2>/dev/null || true
    echo "  Copied linker: $LINKER"
fi

# Create init script for microVM
echo "📝 Creating init script..."
cat > "$ROOTFS_DIR/init" << 'INIT_EOF'
#!/bin/sh

# Mount essential filesystems
mount -t proc proc /proc
mount -t sysfs sys /sys
mount -t devtmpfs dev /dev
mkdir -p /dev/pts
mount -t devpts devpts /dev/pts

# Set hostname
hostname osvm-microvm

# Start OSVM agent
echo "🚀 Starting OSVM Agent in microVM..."
export OSVM_IN_MICROVM=1
export PATH="/usr/local/bin:$PATH"

# Execute OSVM with agent chat
exec /usr/local/bin/osvm agent-chat-v2 --microvm-mode

INIT_EOF

chmod +x "$ROOTFS_DIR/init"

# Create minimal fstab
cat > "$ROOTFS_DIR/etc/fstab" << EOF
proc  /proc  proc  defaults  0  0
sysfs /sys   sysfs defaults  0  0
EOF

# Create a simple inittab (if needed)
cat > "$ROOTFS_DIR/etc/inittab" << EOF
::sysinit:/init
EOF

# Calculate rootfs size (OSVM + libs + Alpine base ~200MB, add 100MB buffer)
ROOTFS_SIZE_MB=300

echo "💾 Creating ext4 filesystem image (${ROOTFS_SIZE_MB}MB)..."
dd if=/dev/zero of="$ROOTFS_IMG" bs=1M count=$ROOTFS_SIZE_MB status=progress

# Format as ext4
mkfs.ext4 -F "$ROOTFS_IMG"

# Mount and copy contents
MOUNT_POINT="$ROOTFS_DIR-mount"
mkdir -p "$MOUNT_POINT"

# Try to mount (this might need user to run with appropriate permissions)
if mount -o loop "$ROOTFS_IMG" "$MOUNT_POINT" 2>/dev/null; then
    echo "📤 Copying rootfs contents..."
    cp -a "$ROOTFS_DIR"/* "$MOUNT_POINT/"
    umount "$MOUNT_POINT"
    rmdir "$MOUNT_POINT"
    echo "✅ Rootfs image created: $ROOTFS_IMG"
else
    echo "❌ Cannot mount image (need loop device support or different permissions)"
    echo "   Trying alternative method with mksquashfs..."
    
    # Alternative: Create squashfs instead (read-only but works without mount)
    SQUASHFS_IMG="${ROOTFS_IMG%.ext4}.squashfs"
    if command -v mksquashfs &> /dev/null; then
        mksquashfs "$ROOTFS_DIR" "$SQUASHFS_IMG" -comp xz
        echo "✅ Created squashfs image: $SQUASHFS_IMG"
        echo "⚠️  Note: Using squashfs (read-only) instead of ext4"
    else
        echo "❌ Neither mount nor mksquashfs available"
        echo "   Created rootfs directory at: $ROOTFS_DIR"
        echo "   You'll need to convert it to an image manually"
    fi
fi

echo ""
echo "🎉 Rootfs build complete!"
echo "   Binary: $ROOTFS_DIR/usr/local/bin/osvm"
echo "   Libraries: $ROOTFS_DIR/lib/"
echo "   Init script: $ROOTFS_DIR/init"
if [ -f "$ROOTFS_IMG" ]; then
    ls -lh "$ROOTFS_IMG"
fi
