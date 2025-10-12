#!/bin/bash

# Script to manage local Solana RPC node

case "$1" in
    start)
        if pgrep -f "solana-test-validator" > /dev/null; then
            echo "❌ Solana test validator is already running"
            exit 1
        fi
        echo "🚀 Starting Solana test validator..."
        nohup solana-test-validator --reset > /tmp/solana-test-validator.log 2>&1 &
        echo "✅ Started with PID: $!"
        echo "📍 RPC URL: http://127.0.0.1:8899"
        echo "🔌 WebSocket URL: ws://127.0.0.1:8900"
        echo "📄 Log file: /tmp/solana-test-validator.log"
        ;;
    stop)
        echo "🛑 Stopping Solana test validator..."
        pkill -f solana-test-validator
        echo "✅ Stopped"
        ;;
    status)
        if pgrep -f "solana-test-validator" > /dev/null; then
            echo "✅ Solana test validator is running"
            echo "📍 RPC URL: http://127.0.0.1:8899"
            echo "🔌 WebSocket URL: ws://127.0.0.1:8900"
            # Check health
            if curl -s -X POST -H "Content-Type: application/json" -d '{"jsonrpc":"2.0","id":1,"method":"getHealth"}' http://127.0.0.1:8899 | grep -q '"result":"ok"'; then
                echo "💚 Health: OK"
            else
                echo "❌ Health: Not responding"
            fi
        else
            echo "❌ Solana test validator is not running"
        fi
        ;;
    logs)
        if [ -f /tmp/solana-test-validator.log ]; then
            tail -f /tmp/solana-test-validator.log
        else
            echo "❌ No log file found"
        fi
        ;;
    *)
        echo "Usage: $0 {start|stop|status|logs}"
        exit 1
        ;;
esac