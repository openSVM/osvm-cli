# Multi-stage build for osvm CLI
FROM rust:1.87-slim AS builder

# Install dependencies for building
RUN apt-get update && apt-get install -y \
    pkg-config \
    libssl-dev \
    libudev-dev \
    perl \
    make \
    g++ \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /app

# Copy dependency files first for better caching
COPY Cargo.toml Cargo.lock ./
COPY rust-toolchain.toml ./

# Copy vendor directory if it exists
COPY vendor/ ./vendor/

# Copy workspace crates (includes ovsm crate)
COPY crates/ ./crates/

# Copy source code
COPY src/ ./src/

# Copy templates directory (required by include_str! macros in audit_templates.rs)
COPY templates/ ./templates/

# Build the application
RUN cargo build --release

# Runtime image
FROM debian:bookworm-slim

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    ca-certificates \
    libudev1 \
    libssl3 \
    && rm -rf /var/lib/apt/lists/*

# Create non-root user
RUN useradd -m -u 1000 osvm

# Copy the binary from builder stage
COPY --from=builder /app/target/release/osvm /usr/local/bin/osvm

# Make it executable
RUN chmod +x /usr/local/bin/osvm

# Switch to non-root user
USER osvm

# Set the entrypoint
ENTRYPOINT ["/usr/local/bin/osvm"]
CMD ["--help"]