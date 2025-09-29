#!/bin/bash

# Test the chat UI with simulated input
echo -e "test message\n/help\n/exit" | timeout 2 ./target/release/osvm agent chat 2>&1 | cat -v
