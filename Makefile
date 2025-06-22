# OSVM CLI Makefile
# Quick commands for building, testing, and installing

.PHONY: help build build-release test install install-dev clean clean-install uninstall

# Default target
help:
	@echo "🚀 OSVM CLI Build & Installation Commands"
	@echo "========================================"
	@echo ""
	@echo "Build Commands:"
	@echo "  make build         - Build debug binary"
	@echo "  make build-release - Build optimized release binary"
	@echo "  make test          - Run all tests"
	@echo ""
	@echo "Installation Commands:"
	@echo "  make install       - Build release and install to /usr/bin (requires sudo)"
	@echo "  make install-dev   - Install debug binary to /usr/bin (requires sudo)"
	@echo "  make uninstall     - Remove osvm from /usr/bin (requires sudo)"
	@echo ""
	@echo "Cleanup Commands:"
	@echo "  make clean         - Clean build artifacts"
	@echo "  make clean-install - Remove installed binaries and backups"
	@echo ""
	@echo "Development Commands:"
	@echo "  make run           - Run the debug binary locally"
	@echo "  make check         - Check code without building"
	@echo "  make fmt           - Format code"
	@echo "  make clippy        - Run clippy linter"

# Build commands
build:
	@echo "🔨 Building debug binary..."
	cargo build

build-release:
	@echo "🔨 Building release binary..."
	cargo build --release

test:
	@echo "🧪 Running tests..."
	cargo test

check:
	@echo "🔍 Checking code..."
	cargo check

fmt:
	@echo "🎨 Formatting code..."
	cargo fmt

clippy:
	@echo "📎 Running clippy..."
	cargo clippy -- -D warnings

# Installation commands
install: build-release
	@echo "📦 Installing release binary..."
	./install-release.sh

install-dev: build
	@echo "📦 Installing debug binary..."
	sudo cp target/debug/osvm /usr/bin/osvm
	sudo chmod +x /usr/bin/osvm
	@echo "✅ Debug binary installed to /usr/bin/osvm"

uninstall:
	@echo "🗑️  Uninstalling osvm..."
	sudo rm -f /usr/bin/osvm
	@echo "✅ osvm removed from /usr/bin"

# Cleanup commands
clean:
	@echo "🧹 Cleaning build artifacts..."
	cargo clean

clean-install: uninstall
	@echo "🧹 Cleaning installed binaries and backups..."
	sudo rm -f /usr/bin/osvm.backup
	@echo "✅ All installed files cleaned"

# Development commands
run:
	@echo "🏃 Running debug binary..."
	cargo run

# Verification command
verify-install:
	@echo "🧪 Verifying installation..."
	@if command -v osvm >/dev/null 2>&1; then \
		echo "✅ osvm is installed and accessible"; \
		echo "📍 Location: $$(which osvm)"; \
		echo "📦 Version: $$(osvm --version)"; \
	else \
		echo "❌ osvm is not installed or not in PATH"; \
		exit 1; \
	fi

# Quick development cycle
dev: clean build test
	@echo "✅ Development build completed"

# Full release cycle
release: clean build-release test install
	@echo "✅ Release build and installation completed"
