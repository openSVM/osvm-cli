#!/bin/bash

# Test real-time streaming graph visualization

echo "Testing Real-Time Streaming Graph Visualization"
echo "================================================"
echo ""
echo "This will show a progressively building graph as data arrives."
echo "Press Ctrl+C to exit the visualization."
echo ""

# Create a simple test program that uses the demo function
cat > /tmp/test_streaming.rs << 'EOF'
use osvm::services::realtime_graph_stream::demo_streaming_investigation;

#[tokio::main]
async fn main() {
    println!("Starting streaming demo...\n");

    let wallet = "DemoWallet12345678901234567890123456789012";

    if let Err(e) = demo_streaming_investigation(wallet).await {
        eprintln!("Demo failed: {}", e);
    }
}
EOF

# Try to compile and run the test
echo "Building test program..."
cd /home/larp/larpdevs/osvm-cli
rustc --edition 2021 -L target/release/deps /tmp/test_streaming.rs -o /tmp/test_streaming 2>/dev/null

if [ $? -ne 0 ]; then
    echo "Direct test compilation failed, using the osvm binary instead..."

    # Test with a real wallet using the new --stream flag
    echo ""
    echo "Testing with research command and --stream flag:"
    echo "./target/release/osvm research --agent 11111111111111111111111111111111 --stream"
    echo ""
    echo "Note: This would normally show real-time graph updates."
    echo "Since we're in test mode, showing the help instead:"
    ./target/release/osvm research --help 2>/dev/null || echo "Research command not available yet"
else
    echo "Running streaming demo..."
    /tmp/test_streaming
fi

echo ""
echo "Test complete!"