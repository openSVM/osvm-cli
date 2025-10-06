#!/bin/bash
set -e

echo "Testing ClickHouse Parser Extraction Rates..."
echo "=============================================="
echo ""

# Query extraction stats
echo "Querying extraction statistics..."
curl -s 'http://localhost:8123/' --data-binary "
SELECT 
    count() as total_rows,
    countIf(length(logs) > 0) as rows_with_logs,
    countIf(length(account_keys) > 0) as rows_with_accounts,
    countIf(length(inner_instructions) > 0) as rows_with_inner_ix,
    round(countIf(length(logs) > 0) * 100.0 / count(), 2) as logs_pct,
    round(countIf(length(account_keys) > 0) * 100.0 / count(), 2) as accounts_pct,
    round(countIf(length(inner_instructions) > 0) * 100.0 / count(), 2) as inner_ix_pct
FROM default.solana_transactions
FORMAT Vertical
"

echo ""
echo "Sampling data quality..."
curl -s 'http://localhost:8123/' --data-binary "
SELECT 
    signature,
    arrayElement(logs, 1) as first_log,
    arrayElement(account_keys, 1) as first_account,
    length(logs) as log_count,
    length(account_keys) as account_count,
    length(inner_instructions) as inner_ix_count
FROM default.solana_transactions
WHERE length(logs) > 0 OR length(account_keys) > 0 OR length(inner_instructions) > 0
LIMIT 5
FORMAT Vertical
"

echo ""
echo "Done!"
