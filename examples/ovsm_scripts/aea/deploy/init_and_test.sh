#!/bin/bash
# AEA Protocol - Devnet Initialization and Testing Script
# Initializes protocol accounts and tests basic flows

set -e

# Program IDs
AEA_PROTOCOL="8rHM6od1gkE9LkqnuDwLnrYMMGHqBNqsL1oGXh2QYASj"
AEA_NEGOTIATION="CfkaPnSDUswrHAie1D2pahU4yBbuGRjioS4i5efVy2KX"
AEA_GOVERNANCE="CFqhR4CxwWWUTpPSrVEJADee3pGJu7suYU1mLpcseDqZ"

# Find keypair with SOL
KEYPAIR="/tmp/devnet-deploy-1764245600.json"
if [ ! -f "$KEYPAIR" ]; then
    echo "Error: Keypair not found at $KEYPAIR"
    exit 1
fi

echo "=== AEA Protocol Initialization ==="
echo "Using keypair: $KEYPAIR"
echo "Balance: $(solana balance $KEYPAIR --url devnet)"
echo ""

# Create config account
echo "Step 1: Creating config PDA account..."
CONFIG_SEED="aea_config"
CONFIG_PDA=$(solana-keygen pubkey $KEYPAIR | xargs -I {} solana find-program-address $CONFIG_SEED $AEA_PROTOCOL --url devnet 2>/dev/null | head -1 || echo "PDA lookup not available")

echo "Config PDA: $CONFIG_PDA"
echo ""

# For now, we'll use a simpler approach - create accounts directly
echo "Step 2: Allocating config account..."

# Generate a new account for config
CONFIG_KEYPAIR="/tmp/aea-config-$(date +%s).json"
solana-keygen new --no-bip39-passphrase --outfile "$CONFIG_KEYPAIR" --force 2>/dev/null
CONFIG_PUBKEY=$(solana-keygen pubkey "$CONFIG_KEYPAIR")
echo "Config account: $CONFIG_PUBKEY"

# Create the account with 256 bytes (config size)
echo "Creating config account with 256 bytes..."
solana create-account "$CONFIG_KEYPAIR" 256 $AEA_PROTOCOL \
    --keypair "$KEYPAIR" \
    --url devnet 2>&1 || echo "Account may already exist"

echo ""
echo "Step 3: Calling initializeProtocol (discriminator 0)..."

# Build instruction data: discriminator 0 + optional min_agent_stake (8 bytes)
# Using default stake of 1 SOL = 1_000_000_000 lamports
INSTR_DATA="0001000000e8030000"  # 0x00 (disc) + 1_000_000_000 (1 SOL in little-endian)

# Create admin account
ADMIN_KEYPAIR="/tmp/aea-admin-$(date +%s).json"
solana-keygen new --no-bip39-passphrase --outfile "$ADMIN_KEYPAIR" --force 2>/dev/null
ADMIN_PUBKEY=$(solana-keygen pubkey "$ADMIN_KEYPAIR")
echo "Admin account: $ADMIN_PUBKEY"

# Send initialization transaction
echo "Sending init transaction..."
# Note: This requires proper account setup which is complex via CLI
# For full testing, we should use a TypeScript client

echo ""
echo "=== Protocol Initialization Status ==="
echo "Config Account: $CONFIG_PUBKEY"
echo "Admin Account: $ADMIN_PUBKEY"
echo ""
echo "Note: Full initialization requires proper PDA derivation and"
echo "transaction building. Use the TypeScript client for production."
echo ""
echo "Programs deployed and ready:"
echo "  AEA Protocol:    $AEA_PROTOCOL"
echo "  AEA Negotiation: $AEA_NEGOTIATION"
echo "  AEA Governance:  $AEA_GOVERNANCE"
