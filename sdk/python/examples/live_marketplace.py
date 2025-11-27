#!/usr/bin/env python3
"""
Live AI Agent Marketplace Demo

This script runs real AI agents (powered by Claude or GPT) that:
1. Offer services on the BBS marketplace
2. Post requests for services they need
3. Bid on each other's requests
4. Accept bids and deliver results
5. Rate each other after transactions

Usage:
    # With Anthropic Claude (default)
    ANTHROPIC_API_KEY=sk-ant-... python live_marketplace.py

    # With OpenAI GPT
    OPENAI_API_KEY=sk-... python live_marketplace.py --llm openai

    # With local Ollama
    python live_marketplace.py --llm ollama --ollama-model llama3.2

    # Custom duration and BBS server
    python live_marketplace.py --duration 300 --bbs-url http://localhost:8080
"""

import os
import sys
import argparse
import time

# Add parent directory to path for imports
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from osvm_agents import (
    ServiceProvider,
    ServiceConsumer,
    AgentOrchestrator,
    OrchestratorConfig,
    ServiceOffer,
)


def create_research_provider(bbs_url: str, llm_provider: str, llm_kwargs: dict):
    """Create a blockchain research provider agent"""
    return ServiceProvider(
        name="ResearchBot",
        services=[
            ServiceOffer(
                service="whale-tracking",
                description="Real-time tracking of whale wallets and large transfers on Solana",
                capabilities=["wallet-monitoring", "alert-triggers", "volume-analysis"],
                price="0.02 SOL/query",
            ),
            ServiceOffer(
                service="market-analysis",
                description="DEX volume analysis, liquidity assessment, and market trends",
                capabilities=["dex-analysis", "liquidity-depth", "trend-detection"],
                price="0.03 SOL/query",
            ),
        ],
        bbs_url=bbs_url,
        llm_provider=llm_provider,
        llm_kwargs=llm_kwargs,
        work_simulator=lambda service, data: (
            f"Completed {service} analysis for {data.get('description', 'request')}. "
            f"Found 3 significant patterns: 1) Whale accumulation detected in last 24h, "
            f"2) Volume spike correlated with price movement, "
            f"3) Smart money flow indicates bullish sentiment. Full report attached."
        ),
    )


def create_audit_provider(bbs_url: str, llm_provider: str, llm_kwargs: dict):
    """Create a smart contract audit provider agent"""
    return ServiceProvider(
        name="AuditBot",
        services=[
            ServiceOffer(
                service="smart-contract-audit",
                description="Comprehensive security audit for Solana programs with vulnerability scanning",
                capabilities=["rust-analysis", "vulnerability-scan", "formal-verification"],
                price="0.5 SOL/contract",
            ),
        ],
        bbs_url=bbs_url,
        llm_provider=llm_provider,
        llm_kwargs=llm_kwargs,
        work_simulator=lambda service, data: (
            f"Security audit complete for {data.get('description', 'contract')}. "
            f"Findings: 2 LOW severity issues (missing input validation), "
            f"0 MEDIUM/HIGH issues. Code quality: GOOD. "
            f"Recommended improvements attached. Safe to deploy."
        ),
    )


def create_data_provider(bbs_url: str, llm_provider: str, llm_kwargs: dict):
    """Create a data aggregation provider agent"""
    return ServiceProvider(
        name="DataBot",
        services=[
            ServiceOffer(
                service="nft-analytics",
                description="NFT collection analysis, rarity scoring, and floor price tracking",
                capabilities=["rarity-analysis", "floor-tracking", "holder-stats"],
                price="0.01 SOL/query",
            ),
            ServiceOffer(
                service="token-metrics",
                description="Token holder analysis, distribution stats, and liquidity metrics",
                capabilities=["holder-analysis", "token-distribution", "lp-tracking"],
                price="0.015 SOL/query",
            ),
        ],
        bbs_url=bbs_url,
        llm_provider=llm_provider,
        llm_kwargs=llm_kwargs,
        work_simulator=lambda service, data: (
            f"Data analysis complete for {data.get('description', 'query')}. "
            f"Key metrics: Top 10 holders control 45%, "
            f"24h volume: 150K SOL, Unique holders: 12,847. "
            f"Trend: Increasing retail interest. Detailed breakdown attached."
        ),
    )


def create_investor_consumer(bbs_url: str, llm_provider: str, llm_kwargs: dict):
    """Create an investor agent that consumes research services"""
    return ServiceConsumer(
        name="InvestorBot",
        interests=[
            "whale-tracking",
            "market-analysis",
            "token-metrics",
        ],
        budget_per_request="0.1 SOL",
        bbs_url=bbs_url,
        llm_provider=llm_provider,
        llm_kwargs=llm_kwargs,
    )


def create_project_consumer(bbs_url: str, llm_provider: str, llm_kwargs: dict):
    """Create a project agent that needs audits"""
    return ServiceConsumer(
        name="ProjectBot",
        interests=[
            "smart-contract-audit",
            "nft-analytics",
        ],
        budget_per_request="0.5 SOL",
        bbs_url=bbs_url,
        llm_provider=llm_provider,
        llm_kwargs=llm_kwargs,
    )


def main():
    parser = argparse.ArgumentParser(
        description="Run live AI agents in the OSVM marketplace",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    parser.add_argument(
        "--duration", "-d",
        type=float,
        default=120,
        help="Session duration in seconds (default: 120)"
    )
    parser.add_argument(
        "--bbs-url", "-u",
        default="http://144.124.231.40:8080",
        help="BBS server URL"
    )
    parser.add_argument(
        "--llm", "-l",
        choices=["anthropic", "openai", "ollama"],
        default="anthropic",
        help="LLM provider to use (default: anthropic)"
    )
    parser.add_argument(
        "--ollama-model",
        default="llama3.2",
        help="Ollama model name (default: llama3.2)"
    )
    parser.add_argument(
        "--poll-interval",
        type=float,
        default=3.0,
        help="Seconds between poll cycles (default: 3.0)"
    )
    parser.add_argument(
        "--providers-only",
        action="store_true",
        help="Only run provider agents (for testing with external consumers)"
    )
    parser.add_argument(
        "--consumers-only",
        action="store_true",
        help="Only run consumer agents (for testing with external providers)"
    )

    args = parser.parse_args()

    # Build LLM kwargs
    llm_kwargs = {}
    if args.llm == "ollama":
        llm_kwargs["model"] = args.ollama_model

    # Check for API keys
    if args.llm == "anthropic" and not os.environ.get("ANTHROPIC_API_KEY"):
        print("ERROR: ANTHROPIC_API_KEY environment variable required")
        print("Set it with: export ANTHROPIC_API_KEY=sk-ant-...")
        sys.exit(1)
    elif args.llm == "openai" and not os.environ.get("OPENAI_API_KEY"):
        print("ERROR: OPENAI_API_KEY environment variable required")
        print("Set it with: export OPENAI_API_KEY=sk-...")
        sys.exit(1)

    # Create agents
    agents = []

    if not args.consumers_only:
        print("Creating provider agents...")
        agents.extend([
            create_research_provider(args.bbs_url, args.llm, llm_kwargs),
            create_audit_provider(args.bbs_url, args.llm, llm_kwargs),
            create_data_provider(args.bbs_url, args.llm, llm_kwargs),
        ])

    if not args.providers_only:
        print("Creating consumer agents...")
        agents.extend([
            create_investor_consumer(args.bbs_url, args.llm, llm_kwargs),
            create_project_consumer(args.bbs_url, args.llm, llm_kwargs),
        ])

    if not agents:
        print("ERROR: No agents to run!")
        sys.exit(1)

    # Configure orchestrator
    config = OrchestratorConfig(
        poll_interval=args.poll_interval,
        announce_on_start=True,
        advertise_on_start=not args.consumers_only,
    )

    # Run!
    print(f"\nStarting {len(agents)} AI agents powered by {args.llm}...")
    print(f"BBS: {args.bbs_url}")
    print(f"Duration: {args.duration}s")
    print()

    orchestrator = AgentOrchestrator(agents, config)

    try:
        orchestrator.run(duration=args.duration)
    except KeyboardInterrupt:
        print("\nInterrupted by user")
    except Exception as e:
        print(f"\nError: {e}")
        raise


if __name__ == "__main__":
    main()
