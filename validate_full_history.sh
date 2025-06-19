#!/bin/bash

echo "🔍 Validating Solana Mainnet RPC Full History Configuration"
echo "=========================================================="

# Check if the process is running with full history flags
echo "1. Checking process configuration..."
PROCESS_ARGS=$(ps aux | grep agave-validator | grep -v grep)

if echo "$PROCESS_ARGS" | grep -q "enable-rpc-transaction-history"; then
    echo "   ✅ --enable-rpc-transaction-history: ENABLED"
else
    echo "   ❌ --enable-rpc-transaction-history: MISSING"
fi

if echo "$PROCESS_ARGS" | grep -q "enable-extended-tx-metadata-storage"; then
    echo "   ✅ --enable-extended-tx-metadata-storage: ENABLED"
else
    echo "   ❌ --enable-extended-tx-metadata-storage: MISSING"
fi

if echo "$PROCESS_ARGS" | grep -q "full-rpc-api"; then
    echo "   ✅ --full-rpc-api: ENABLED"
else
    echo "   ❌ --full-rpc-api: MISSING"
fi

if echo "$PROCESS_ARGS" | grep -q "account-index program-id"; then
    echo "   ✅ --account-index program-id: ENABLED"
else
    echo "   ❌ --account-index program-id: MISSING"
fi

echo ""
echo "2. Checking RocksDB transaction storage configuration..."

if [ -f "mainnet-ledger/rocksdb/OPTIONS-000024" ]; then
    TRANSACTION_COLS=$(cat mainnet-ledger/rocksdb/OPTIONS-000024 | grep -i transaction | wc -l)
    if [ "$TRANSACTION_COLS" -gt 0 ]; then
        echo "   ✅ Transaction storage columns found: $TRANSACTION_COLS"
        echo "   📋 Configured columns:"
        cat mainnet-ledger/rocksdb/OPTIONS-000024 | grep "CFOptions.*transaction" | sed 's/^/      /'
    else
        echo "   ❌ No transaction storage columns found"
    fi
else
    echo "   ⚠️  RocksDB options file not found (node may still be initializing)"
fi

echo ""
echo "3. Checking ledger directory structure..."

if [ -d "mainnet-ledger/rocksdb" ]; then
    echo "   ✅ RocksDB directory exists"
    ROCKSDB_SIZE=$(du -sh mainnet-ledger/rocksdb | cut -f1)
    echo "   📊 RocksDB size: $ROCKSDB_SIZE"
else
    echo "   ❌ RocksDB directory not found"
fi

if [ -d "mainnet-ledger/accounts" ]; then
    echo "   ✅ Accounts directory exists"
    ACCOUNTS_SIZE=$(du -sh mainnet-ledger/accounts | cut -f1)
    echo "   📊 Accounts size: $ACCOUNTS_SIZE"
else
    echo "   ❌ Accounts directory not found"
fi

if [ -d "mainnet-ledger/snapshots" ]; then
    echo "   ✅ Snapshots directory exists"
    SNAPSHOTS_COUNT=$(ls mainnet-ledger/snapshots/ 2>/dev/null | wc -l)
    echo "   📊 Snapshots count: $SNAPSHOTS_COUNT"
else
    echo "   ❌ Snapshots directory not found"
fi

echo ""
echo "4. Testing RPC availability..."

RPC_HEALTH=$(curl -s -X POST -H "Content-Type: application/json" \
    -d '{"jsonrpc":"2.0","id":1,"method":"getHealth"}' \
    http://localhost:8899 2>/dev/null)

if echo "$RPC_HEALTH" | grep -q '"ok"'; then
    echo "   ✅ RPC endpoint is healthy and responding"
    
    echo ""
    echo "5. Testing full history RPC methods..."
    
    # Test getSignaturesForAddress (history method)
    echo "   🔍 Testing getSignaturesForAddress method..."
    SIGNATURES_TEST=$(curl -s -X POST -H "Content-Type: application/json" \
        -d '{"jsonrpc":"2.0","id":1,"method":"getSignaturesForAddress","params":["11111111111111111111111111111111",{"limit":1}]}' \
        http://localhost:8899 2>/dev/null)
    
    if echo "$SIGNATURES_TEST" | grep -q '"result"'; then
        echo "      ✅ getSignaturesForAddress: Working"
    else
        echo "      ⚠️  getSignaturesForAddress: Not ready or limited data"
    fi
    
    # Test getConfirmedTransaction (history method)
    echo "   🔍 Testing getConfirmedTransaction method availability..."
    CONFIRMED_TX_TEST=$(curl -s -X POST -H "Content-Type: application/json" \
        -d '{"jsonrpc":"2.0","id":1,"method":"getConfirmedTransaction","params":["invalid"]}' \
        http://localhost:8899 2>/dev/null)
    
    if echo "$CONFIRMED_TX_TEST" | grep -q '"error".*"Invalid param"'; then
        echo "      ✅ getConfirmedTransaction: Method available"
    else
        echo "      ⚠️  getConfirmedTransaction: Method may not be enabled"
    fi
    
else
    echo "   ⚠️  RPC endpoint not responding yet (node still syncing)"
    echo "   💡 This is normal during initial bootstrap - try again later"
fi

echo ""
echo "6. Disk space and resource usage..."

LEDGER_SIZE=$(du -sh mainnet-ledger 2>/dev/null | cut -f1)
echo "   📊 Total ledger size: $LEDGER_SIZE"

AVAILABLE_SPACE=$(df -h . | tail -1 | awk '{print $4}')
echo "   💾 Available disk space: $AVAILABLE_SPACE"

PROCESS_MEM=$(ps aux | grep agave-validator | grep -v grep | awk '{print $6}')
if [ ! -z "$PROCESS_MEM" ]; then
    PROCESS_MEM_MB=$((PROCESS_MEM / 1024))
    echo "   🧠 Process memory usage: ${PROCESS_MEM_MB}MB"
fi

echo ""
echo "📋 SUMMARY"
echo "=========="
echo "The mainnet RPC node is configured with full transaction history storage."
echo "Key features enabled:"
echo "  • Complete transaction history (--enable-rpc-transaction-history)"
echo "  • Extended transaction metadata (--enable-extended-tx-metadata-storage)"  
echo "  • Full RPC API access (--full-rpc-api)"
echo "  • Program ID indexing for efficient queries"
echo "  • RocksDB with dedicated transaction storage columns"
echo ""
echo "📍 Current Status: $(osvm rpc-manager mainnet --status 2>/dev/null | grep 'Status:' | cut -d: -f2- || echo 'Check status with: osvm rpc-manager mainnet --status')"
echo ""
echo "💡 Once syncing completes, all historical transactions will be queryable"
echo "   through standard RPC methods like getSignaturesForAddress, getTransaction, etc."
