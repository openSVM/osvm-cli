#!/usr/bin/env python3
"""
Economic AI Agent Marketplace

Real AI agents with:
1. Actual SOL wallet balances (simulated)
2. Real blockchain queries (Solana RPC)
3. P&L tracking and economic constraints
4. LLM-powered decision making

Usage:
    # With Ollama (local)
    python economic_marketplace.py --llm ollama --duration 60

    # With Claude (smarter decisions)
    ANTHROPIC_API_KEY=sk-ant-... python economic_marketplace.py --llm anthropic

    # Watch specific wallet behavior
    python economic_marketplace.py --show-wallets
"""

import os
import sys
import argparse
import time
import json
from typing import Optional

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from osvm_agents import (
    ServiceProvider,
    ServiceConsumer,
    AgentOrchestrator,
    OrchestratorConfig,
    ServiceOffer,
)
from osvm_agents.wallet import WalletManager, AgentWallet
from osvm_agents.blockchain import (
    create_whale_tracker,
    create_token_analyzer,
    create_security_auditor,
    BlockchainAnalyzer,
)


class EconomicServiceProvider(ServiceProvider):
    """
    Service provider with wallet integration

    Tracks earnings, makes economically rational decisions based on:
    - Current balance
    - Historical P&L
    - Market conditions
    """

    def __init__(self, wallet: AgentWallet, wallet_manager: "WalletManager", **kwargs):
        super().__init__(**kwargs)
        self.wallet = wallet
        self.wallet_manager = wallet_manager
        self.pending_prices: dict = {}  # request_id -> price

    @property
    def system_prompt(self) -> str:
        wallet_info = self.wallet.get_summary()
        base_prompt = super().system_prompt
        return f"""{base_prompt}

ECONOMIC CONTEXT:
- Your current balance: {wallet_info['balance']:.3f} SOL
- Total earned: {wallet_info['total_earned']:.3f} SOL
- P&L: {'+' if wallet_info['pnl'] >= 0 else ''}{wallet_info['pnl']:.3f} SOL
- Pending escrows: {wallet_info['pending_escrows']}

Make economically rational decisions. If you're losing money, consider:
- Raising prices
- Being more selective about which jobs to bid on
- Improving delivery quality for better ratings

Your goal is to maximize profit while maintaining reputation."""

    def process_message(self, msg) -> "Optional[Post]":
        """Override to handle payments"""
        from osvm_agents.agent import MessageType as MT

        if msg.type.value == "ACCEPT":
            return self._handle_accept_with_payment(msg)
        elif msg.type.value == "REQUEST":
            return self._handle_request(msg)
        return None

    def _handle_accept_with_payment(self, msg) -> "Optional[Post]":
        """Handle when our bid is accepted - receive payment and deliver"""
        from osvm_agents.agent import MessageType
        import time

        accept_data = msg.data
        request_id = accept_data.get("request_id")
        accepted_agent = accept_data.get("agent")

        if accepted_agent != self.name:
            return None

        if request_id not in self.state.active_bids:
            return None

        bid_info = self.state.active_bids.pop(request_id)
        request_data = bid_info["request_data"]

        # Get price from accept message or default
        price = accept_data.get("price", 0.05)
        if isinstance(price, str):
            price = float(price.replace(" SOL", "").strip())

        # Transfer payment
        if self.wallet_manager.transfer(
            msg.agent, self.name, price, request_id,
            request_data.get("need", "service")
        ):
            print(f"    💰 {msg.agent} paid {price} SOL to {self.name}")

        # Do the work
        service = request_data.get("need", "service")
        result = self.work_simulator(service, request_data)

        # Deliver
        delivery_data = {
            "type": "DELIVER",
            "request_id": request_id,
            "result": result[:200] + "..." if len(result) > 200 else result,
            "proof": f"sig_{int(time.time())}",
        }

        self.state.pending_deliveries[request_id] = {
            "delivery_data": delivery_data,
            "requester": msg.agent,
        }

        post = self.post(MessageType.DELIVER, delivery_data)

        # Rate the client
        time.sleep(0.3)
        rating_data = {
            "type": "RATE",
            "agent": msg.agent,
            "transaction_id": request_id,
            "score": 5,
            "comment": "Great client!",
        }
        self.post(MessageType.RATE, rating_data)

        self.state.completed_transactions.append({
            "request_id": request_id,
            "role": "provider",
            "client": msg.agent,
            "earned": price,
        })

        return post


class EconomicServiceConsumer(ServiceConsumer):
    """
    Service consumer with wallet integration

    Makes purchasing decisions based on:
    - Available budget
    - Value received vs cost
    - Provider reputation
    """

    def __init__(self, wallet: AgentWallet, wallet_manager: "WalletManager", **kwargs):
        super().__init__(**kwargs)
        self.wallet = wallet
        self.wallet_manager = wallet_manager

    @property
    def system_prompt(self) -> str:
        wallet_info = self.wallet.get_summary()
        base_prompt = super().system_prompt
        return f"""{base_prompt}

ECONOMIC CONTEXT:
- Your current balance: {wallet_info['balance']:.3f} SOL
- Total spent: {wallet_info['total_spent']:.3f} SOL
- Transactions completed: {wallet_info['transaction_count']}

You have LIMITED FUNDS. Make wise purchasing decisions:
- Don't overpay for services
- Negotiate if prices seem high
- Only buy services you actually need
- If balance is low, be very selective

Your goal is to get maximum value from your budget."""

    def process_message(self, msg) -> "Optional[Post]":
        """Override to handle budget checking"""
        from osvm_agents.agent import MessageType as MT

        if msg.type.value == "BID":
            return self._handle_bid_with_budget(msg)
        elif msg.type.value == "DELIVER":
            return self._handle_delivery(msg)
        elif msg.type.value == "OFFER":
            return self._consider_offer(msg)
        return None

    def _handle_bid_with_budget(self, msg) -> "Optional[Post]":
        """Handle incoming bid - check budget before accepting"""
        from osvm_agents.agent import MessageType

        bid_data = msg.data
        request_id = bid_data.get("request_id")

        if request_id not in self.pending_requests:
            return None

        # Parse the price from the bid
        offer = bid_data.get("offer", "0")
        if isinstance(offer, str):
            price = float(offer.replace(" SOL", "").strip())
        else:
            price = float(offer)

        # Check if we can afford it
        if not self.wallet.can_afford(price):
            print(f"    ❌ {self.name} cannot afford {price:.3f} SOL (balance: {self.wallet.balance:.3f})")
            return None

        # Store bid
        self.received_bids[request_id].append(msg)

        # Simple acceptance logic - accept if we can afford it
        request_data = self.pending_requests.pop(request_id, {})

        accept_data = {
            "type": "ACCEPT",
            "bid_id": bid_data.get("id"),
            "agent": msg.agent,
            "request_id": request_id,
            "price": price,
        }

        self.state.completed_transactions.append({
            "request_id": request_id,
            "role": "consumer",
            "provider": msg.agent,
            "paid": price,
        })

        print(f"    ✅ {self.name} accepts bid from {msg.agent} for {price:.3f} SOL")
        return self.post(MessageType.ACCEPT, accept_data)


def create_economic_agents(
    bbs_url: str,
    llm_provider: str,
    llm_kwargs: dict,
    wallet_manager: WalletManager,
):
    """Create agents with wallet integration and real blockchain work"""

    agents = []

    # === PROVIDERS ===

    # ResearchBot - Whale tracking specialist
    research_wallet = wallet_manager.create_wallet("ResearchBot", initial_balance=5.0)
    researcher = EconomicServiceProvider(
        wallet=research_wallet,
        wallet_manager=wallet_manager,
        name="ResearchBot",
        services=[
            ServiceOffer(
                service="whale-tracking",
                description="Real-time tracking of whale wallets with on-chain data analysis",
                capabilities=["wallet-monitoring", "flow-analysis", "accumulation-detection"],
                price="0.05 SOL/query",
            ),
        ],
        bbs_url=bbs_url,
        llm_provider=llm_provider,
        llm_kwargs=llm_kwargs,
        work_simulator=create_whale_tracker(),  # REAL blockchain queries
    )
    agents.append(researcher)

    # AuditBot - Security specialist
    audit_wallet = wallet_manager.create_wallet("AuditBot", initial_balance=5.0)
    auditor = EconomicServiceProvider(
        wallet=audit_wallet,
        wallet_manager=wallet_manager,
        name="AuditBot",
        services=[
            ServiceOffer(
                service="security-audit",
                description="On-chain security analysis of Solana programs",
                capabilities=["upgradeability-check", "authority-verification", "risk-assessment"],
                price="0.2 SOL/audit",
            ),
        ],
        bbs_url=bbs_url,
        llm_provider=llm_provider,
        llm_kwargs=llm_kwargs,
        work_simulator=create_security_auditor(),  # REAL security checks
    )
    agents.append(auditor)

    # DataBot - Token analytics
    data_wallet = wallet_manager.create_wallet("DataBot", initial_balance=5.0)
    data_provider = EconomicServiceProvider(
        wallet=data_wallet,
        wallet_manager=wallet_manager,
        name="DataBot",
        services=[
            ServiceOffer(
                service="token-analysis",
                description="Token holder distribution and supply analysis",
                capabilities=["holder-analysis", "supply-metrics", "risk-factors"],
                price="0.03 SOL/analysis",
            ),
        ],
        bbs_url=bbs_url,
        llm_provider=llm_provider,
        llm_kwargs=llm_kwargs,
        work_simulator=create_token_analyzer(),  # REAL token analysis
    )
    agents.append(data_provider)

    # === CONSUMERS ===

    # InvestorBot - Research consumer with budget
    investor_wallet = wallet_manager.create_wallet("InvestorBot", initial_balance=2.0)
    investor = EconomicServiceConsumer(
        wallet=investor_wallet,
        wallet_manager=wallet_manager,
        name="InvestorBot",
        interests=["whale-tracking", "token-analysis"],
        budget_per_request="0.1 SOL",
        bbs_url=bbs_url,
        llm_provider=llm_provider,
        llm_kwargs=llm_kwargs,
    )
    agents.append(investor)

    # ProjectBot - Security consumer
    project_wallet = wallet_manager.create_wallet("ProjectBot", initial_balance=1.5)
    project = EconomicServiceConsumer(
        wallet=project_wallet,
        wallet_manager=wallet_manager,
        name="ProjectBot",
        interests=["security-audit"],
        budget_per_request="0.3 SOL",
        bbs_url=bbs_url,
        llm_provider=llm_provider,
        llm_kwargs=llm_kwargs,
    )
    agents.append(project)

    return agents


def run_economic_marketplace(
    duration: float = 120,
    bbs_url: str = "http://144.124.231.40:8080",
    llm_provider: str = "ollama",
    llm_kwargs: Optional[dict] = None,
    show_wallets: bool = True,
):
    """Run the economic marketplace simulation"""

    llm_kwargs = llm_kwargs or {}

    print("=" * 70)
    print("ECONOMIC AI AGENT MARKETPLACE")
    print("=" * 70)
    print(f"BBS: {bbs_url}")
    print(f"LLM: {llm_provider}")
    print(f"Duration: {duration}s")
    print()

    # Create wallet manager
    wallet_manager = WalletManager(default_balance=5.0)

    # Get current max post ID so we only process new messages
    import requests as req
    try:
        resp = req.get(f"{bbs_url}/api/boards/MARKETPLACE/posts?limit=1", timeout=5)
        if resp.status_code == 200:
            data = resp.json().get("data", [])
            current_max_id = data[0]["id"] if data else 0
            print(f"Starting from post ID: {current_max_id} (skipping old messages)")
        else:
            current_max_id = 0
    except Exception:
        current_max_id = 0

    # Create agents with wallets
    print("Creating agents with wallets...")
    agents = create_economic_agents(bbs_url, llm_provider, llm_kwargs, wallet_manager)

    # Set last_seen_id for all agents to skip old messages
    for agent in agents:
        agent.state.last_seen_id = current_max_id

    # Show initial wallet state
    if show_wallets:
        print("\nINITIAL WALLET STATE:")
        wallet_manager.print_summary()

    # Configure orchestrator
    config = OrchestratorConfig(
        poll_interval=4.0,  # Slower to allow blockchain queries
        announce_on_start=True,
        advertise_on_start=True,
    )

    # Run orchestrator
    orchestrator = AgentOrchestrator(agents, config)

    try:
        orchestrator.run(duration=duration)
    except KeyboardInterrupt:
        print("\nInterrupted")

    # Show final wallet state
    if show_wallets:
        print("\nFINAL WALLET STATE:")
        wallet_manager.print_summary()

        print("\nLEADERBOARD (by P&L):")
        print("-" * 50)
        for i, entry in enumerate(wallet_manager.get_leaderboard(), 1):
            pnl_sign = "+" if entry["pnl"] >= 0 else ""
            print(
                f"  {i}. {entry['agent']:12} | "
                f"Balance: {entry['balance']:6.3f} | "
                f"P&L: {pnl_sign}{entry['pnl']:.3f}"
            )


def test_blockchain_queries():
    """Test that real blockchain queries work"""
    print("Testing real blockchain queries...")
    print("=" * 50)

    analyzer = BlockchainAnalyzer()

    # Test whale tracking
    print("\n1. Whale Tracking:")
    whale = "9WzDXwBbmPdCBocccc4rTG7sZDQVtTqKqNbW6tKy6Dho"
    result = analyzer.track_whale(whale)
    print(json.dumps(result, indent=2))

    # Test token analysis
    print("\n2. Token Analysis (USDC):")
    usdc = "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"
    result = analyzer.analyze_token_holders(usdc)
    print(json.dumps(result, indent=2))

    # Test security check
    print("\n3. Security Check (Jupiter):")
    jupiter = "JUP4Fb2cqiRUcaTHdrPC8h2gNsA2ETXiPDD33WcGuJB"
    result = analyzer.basic_security_check(jupiter)
    print(json.dumps(result, indent=2))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Economic AI Agent Marketplace")
    parser.add_argument("--duration", "-d", type=float, default=90, help="Duration in seconds")
    parser.add_argument("--bbs-url", "-u", default="http://144.124.231.40:8080")
    parser.add_argument("--llm", "-l", choices=["anthropic", "openai", "ollama"], default="ollama")
    parser.add_argument("--ollama-model", default="smollm2:latest")
    parser.add_argument("--show-wallets", "-w", action="store_true", default=True)
    parser.add_argument("--test-blockchain", action="store_true", help="Test blockchain queries only")

    args = parser.parse_args()

    if args.test_blockchain:
        test_blockchain_queries()
    else:
        llm_kwargs = {}
        if args.llm == "ollama":
            llm_kwargs["model"] = args.ollama_model

        # Check API keys
        if args.llm == "anthropic" and not os.environ.get("ANTHROPIC_API_KEY"):
            print("ERROR: Set ANTHROPIC_API_KEY")
            sys.exit(1)

        run_economic_marketplace(
            duration=args.duration,
            bbs_url=args.bbs_url,
            llm_provider=args.llm,
            llm_kwargs=llm_kwargs,
            show_wallets=args.show_wallets,
        )
