#!/bin/bash
# OSVM MicroVM Environment Setup Script
# This script downloads and configures all required components for OSVM microVM bootstrap

set -e

echo "🚀 OSVM MicroVM Environment Setup"
echo "=================================="
echo ""

# Configuration
OSVM_DIR="$HOME/.osvm"
FIRECRACKER_VERSION="v1.6.0"
KERNEL_VERSION="5.10"

# Create directories
echo "📁 Creating directory structure..."
mkdir -p "$OSVM_DIR/bin"
mkdir -p "$OSVM_DIR/kernel"
mkdir -p "$OSVM_DIR/rootfs"
mkdir -p "$OSVM_DIR/images"

# Download Firecracker binary
echo "📥 Downloading Firecracker $FIRECRACKER_VERSION..."
ARCH=$(uname -m)
FIRECRACKER_URL="https://github.com/firecracker-microvm/firecracker/releases/download/${FIRECRACKER_VERSION}/firecracker-${FIRECRACKER_VERSION}-${ARCH}.tgz"

if ! curl -L -o /tmp/firecracker.tgz "$FIRECRACKER_URL"; then
    echo "❌ Failed to download Firecracker from $FIRECRACKER_URL"
    echo "   Trying alternative download method..."
    
    # Try direct binary download
    FIRECRACKER_BIN_URL="https://github.com/firecracker-microvm/firecracker/releases/download/${FIRECRACKER_VERSION}/firecracker-${FIRECRACKER_VERSION}-${ARCH}"
    if curl -L -o "$OSVM_DIR/bin/firecracker" "$FIRECRACKER_BIN_URL"; then
        chmod +x "$OSVM_DIR/bin/firecracker"
        echo "✅ Firecracker binary downloaded"
    else
        echo "❌ Failed to download Firecracker. Please install manually:"
        echo "   https://github.com/firecracker-microvm/firecracker/blob/main/docs/getting-started.md"
        exit 1
    fi
else
    tar -xzf /tmp/firecracker.tgz -C /tmp/
    mv /tmp/release-${FIRECRACKER_VERSION}-${ARCH}/firecracker-${FIRECRACKER_VERSION}-${ARCH} "$OSVM_DIR/bin/firecracker"
    chmod +x "$OSVM_DIR/bin/firecracker"
    rm -rf /tmp/firecracker.tgz /tmp/release-${FIRECRACKER_VERSION}-${ARCH}
    echo "✅ Firecracker installed"
fi

# Download Linux kernel
echo "📥 Downloading Linux kernel..."
KERNEL_URL="https://s3.amazonaws.com/spec.ccfc.min/firecracker-ci/v1.6/${ARCH}/vmlinux-${KERNEL_VERSION}.bin"
if curl -L -o "$OSVM_DIR/kernel/vmlinux.bin" "$KERNEL_URL"; then
    echo "✅ Kernel downloaded"
else
    echo "⚠️  Failed to download pre-built kernel from S3"
    echo "   Trying alternative kernel source..."
    # Alternative: Amazon Linux kernel
    ALT_KERNEL_URL="https://s3.amazonaws.com/spec.ccfc.min/img/quickstart_guide/${ARCH}/kernels/vmlinux.bin"
    if curl -L -o "$OSVM_DIR/kernel/vmlinux.bin" "$ALT_KERNEL_URL"; then
        echo "✅ Alternative kernel downloaded"
    else
        echo "❌ Failed to download kernel. You'll need to provide your own kernel image."
        echo "   Place it at: $OSVM_DIR/kernel/vmlinux.bin"
    fi
fi

# Create minimal rootfs information file
echo "📝 Creating rootfs build instructions..."
cat > "$OSVM_DIR/rootfs/README.md" << 'EOF'
# OSVM MicroVM Rootfs

This directory should contain the root filesystem image for OSVM microVM.

## Quick Build with Alpine Linux

```bash
# Create 1GB ext4 image
dd if=/dev/zero of=$HOME/.osvm/rootfs/osvm-runtime.ext4 bs=1M count=1024
mkfs.ext4 $HOME/.osvm/rootfs/osvm-runtime.ext4

# Mount it
mkdir -p /tmp/osvm-rootfs
mount -o loop $HOME/.osvm/rootfs/osvm-runtime.ext4 /tmp/osvm-rootfs

# Download Alpine mini rootfs
curl -Lo /tmp/alpine.tar.gz \
  https://dl-cdn.alpinelinux.org/alpine/v3.19/releases/x86_64/alpine-minirootfs-3.19.0-x86_64.tar.gz

# Extract Alpine
tar -xzf /tmp/alpine.tar.gz -C /tmp/osvm-rootfs

# Install Node.js for osvm-mcp
chroot /tmp/osvm-rootfs /bin/sh -c "apk add --no-cache nodejs npm git"

# Copy OSVM binary (build it statically first)
cp target/x86_64-unknown-linux-musl/release/osvm /tmp/osvm-rootfs/usr/local/bin/

# Clone and build osvm-mcp
chroot /tmp/osvm-rootfs /bin/sh -c "
  cd /root
  git clone https://github.com/openSVM/osvm-mcp
  cd osvm-mcp
  npm install
  npm run build
"

# Create init script
cat > /tmp/osvm-rootfs/init << 'INIT'
#!/bin/sh
mount -t proc proc /proc
mount -t sysfs sys /sys
mount -t devtmpfs dev /dev
export OSVM_IN_MICROVM=1
export OSVM_SKIP_MICROVM=1
cd /root
/usr/local/bin/osvm
INIT

chmod +x /tmp/osvm-rootfs/init

# Unmount
umount /tmp/osvm-rootfs
```

## Pre-built Rootfs (Recommended)

For convenience, use the pre-built OSVM rootfs image:
```bash
curl -Lo $HOME/.osvm/rootfs/osvm-runtime.ext4 \
  https://github.com/opensvm/osvm-images/releases/latest/download/osvm-runtime.ext4
```

(Note: This URL is a placeholder - you'll need to build and host this image)
EOF

echo "✅ Rootfs build instructions created"

# Add to PATH
echo ""
echo "📝 Adding Firecracker to PATH..."
if ! grep -q "$OSVM_DIR/bin" "$HOME/.bashrc"; then
    echo "export PATH=\"$OSVM_DIR/bin:\$PATH\"" >> "$HOME/.bashrc"
    echo "✅ Added to ~/.bashrc"
fi

if ! grep -q "$OSVM_DIR/bin" "$HOME/.zshrc" 2>/dev/null; then
    echo "export PATH=\"$OSVM_DIR/bin:\$PATH\"" >> "$HOME/.zshrc" 2>/dev/null || true
fi

# Verify installation
echo ""
echo "🔍 Verifying installation..."
export PATH="$OSVM_DIR/bin:$PATH"

if [ -x "$OSVM_DIR/bin/firecracker" ]; then
    echo "✅ Firecracker: $("$OSVM_DIR/bin/firecracker" --version 2>&1 | head -n1)"
else
    echo "❌ Firecracker: Not found"
fi

if [ -f "$OSVM_DIR/kernel/vmlinux.bin" ]; then
    KERNEL_SIZE=$(ls -lh "$OSVM_DIR/kernel/vmlinux.bin" | awk '{print $5}')
    echo "✅ Kernel: vmlinux.bin ($KERNEL_SIZE)"
else
    echo "❌ Kernel: Not found"
fi

# Summary
echo ""
echo "📊 Installation Summary"
echo "======================="
echo "OSVM Directory: $OSVM_DIR"
echo "Firecracker: $OSVM_DIR/bin/firecracker"
echo "Kernel: $OSVM_DIR/kernel/vmlinux.bin"
echo "Rootfs: $OSVM_DIR/rootfs/ (build required)"
echo ""
echo "⚠️  IMPORTANT: You still need to:"
echo "1. Build OSVM statically: cargo build --release --target x86_64-unknown-linux-musl"
echo "2. Create rootfs image (see $OSVM_DIR/rootfs/README.md)"
echo "3. Update OSVM code to use $OSVM_DIR paths instead of /var/osvm"
echo ""
echo "💡 To use Firecracker immediately:"
echo "   export PATH=\"$OSVM_DIR/bin:\$PATH\""
echo "   Or restart your shell to apply changes"
echo ""
echo "✨ Setup complete!"
