#!/usr/bin/env python3
"""
Multi-Agent BBS Communication Test

5 specialized agents communicate via the BBS to collaborate on tasks:
1. Coordinator - Assigns tasks and monitors progress
2. Researcher - Looks up information and reports findings
3. Analyst - Analyzes data and provides insights
4. Validator - Validates results and flags issues
5. Reporter - Summarizes and creates final reports

Each agent posts to the BBS, reads messages from others, and responds.
"""

import requests
import json
import time
import random
import threading
from datetime import datetime
from typing import Optional, List, Dict

# BBS Server - use the public bootstrap node
BBS_URL = "http://144.124.231.40:8080"
BOARD = "RESEARCH"  # Use RESEARCH board for agent collaboration

class BBSAgent:
    """Base class for BBS-communicating agents"""

    def __init__(self, name: str, role: str, node_id: str):
        self.name = name
        self.role = role
        self.node_id = node_id
        self.last_seen_id = 0
        self.messages_sent = 0
        self.messages_read = 0

    def post(self, message: str, reply_to: Optional[int] = None) -> Optional[Dict]:
        """Post a message to the BBS"""
        try:
            if reply_to:
                url = f"{BBS_URL}/api/posts/{reply_to}/reply"
                data = {"message": f"[{self.name}] {message}", "user_node_id": self.node_id}
            else:
                url = f"{BBS_URL}/api/boards/{BOARD}/posts"
                data = {"message": f"[{self.name}] {message}", "user_node_id": self.node_id}

            resp = requests.post(url, json=data, timeout=10)
            if resp.status_code in [200, 201]:
                self.messages_sent += 1
                result = resp.json()
                return result.get("data", {})
        except Exception as e:
            print(f"  [{self.name}] Post error: {e}")
        return None

    def read_new_messages(self) -> List[Dict]:
        """Read new messages from the board"""
        try:
            resp = requests.get(f"{BBS_URL}/api/boards/{BOARD}/posts?limit=50", timeout=10)
            if resp.status_code == 200:
                data = resp.json()
                messages = data.get("data", [])
                # Filter to only new messages
                new_messages = [m for m in messages if m["id"] > self.last_seen_id]
                if new_messages:
                    self.last_seen_id = max(m["id"] for m in new_messages)
                    self.messages_read += len(new_messages)
                return new_messages
        except Exception as e:
            print(f"  [{self.name}] Read error: {e}")
        return []

    def find_messages_for_me(self, messages: List[Dict]) -> List[Dict]:
        """Find messages that mention this agent or are relevant to its role"""
        relevant = []
        for msg in messages:
            body = msg.get("body", "").lower()
            # Skip own messages
            if f"[{self.name}]".lower() in body:
                continue
            # Check if mentioned by name or role
            if self.name.lower() in body or self.role.lower() in body:
                relevant.append(msg)
        return relevant


class CoordinatorAgent(BBSAgent):
    """Coordinates tasks and monitors progress"""

    def __init__(self):
        super().__init__("Coordinator", "coordinator", "!coord001")
        self.tasks_assigned = 0
        self.tasks_completed = 0

    def assign_task(self, task: str, assignee: str) -> Optional[Dict]:
        """Assign a task to another agent"""
        msg = f"TASK for {assignee}: {task}"
        result = self.post(msg)
        if result:
            self.tasks_assigned += 1
        return result

    def check_progress(self, messages: List[Dict]) -> List[str]:
        """Check for completed tasks in messages"""
        completed = []
        for msg in messages:
            body = msg.get("body", "")
            if "COMPLETED:" in body or "RESULT:" in body or "REPORT:" in body:
                completed.append(body)
                self.tasks_completed += 1
        return completed

    def run_cycle(self):
        """Run one coordination cycle"""
        messages = self.read_new_messages()
        completed = self.check_progress(messages)
        return len(completed)


class ResearcherAgent(BBSAgent):
    """Researches information and reports findings"""

    def __init__(self):
        super().__init__("Researcher", "researcher", "!res001")
        self.research_topics = [
            "Solana TPS metrics for last 24 hours",
            "Top DEX volumes on Raydium",
            "Recent NFT collection launches",
            "Validator stake distribution",
            "MEV activity patterns"
        ]

    def research(self, topic: str) -> str:
        """Simulate researching a topic"""
        # Simulate research time
        time.sleep(random.uniform(0.5, 1.5))
        findings = [
            f"Found 3 relevant data points about {topic}",
            f"Analysis shows trending patterns in {topic}",
            f"Data indicates significant activity in {topic}",
            f"Research complete: {topic} shows normal metrics",
            f"Interesting findings about {topic}: above average"
        ]
        return random.choice(findings)

    def run_cycle(self):
        """Run one research cycle"""
        messages = self.read_new_messages()
        relevant = self.find_messages_for_me(messages)

        for msg in relevant:
            if "TASK" in msg.get("body", ""):
                # Extract task and research
                topic = random.choice(self.research_topics)
                finding = self.research(topic)
                self.post(f"RESULT: {finding}", reply_to=msg["id"])
                return 1
        return 0


class AnalystAgent(BBSAgent):
    """Analyzes data and provides insights"""

    def __init__(self):
        super().__init__("Analyst", "analyst", "!anly001")

    def analyze(self, data: str) -> str:
        """Simulate analyzing data"""
        time.sleep(random.uniform(0.3, 1.0))
        insights = [
            "Pattern analysis reveals bullish trend",
            "Anomaly detected: unusual volume spike",
            "Correlation found between metrics",
            "Risk assessment: moderate exposure",
            "Prediction model suggests stability"
        ]
        return random.choice(insights)

    def run_cycle(self):
        """Run one analysis cycle"""
        messages = self.read_new_messages()
        relevant = self.find_messages_for_me(messages)

        # Also respond to RESULT messages
        for msg in messages:
            body = msg.get("body", "")
            if "RESULT:" in body and "[Analyst]" not in body:
                insight = self.analyze(body)
                self.post(f"ANALYSIS: {insight}", reply_to=msg["id"])
                return 1
        return 0


class ValidatorAgent(BBSAgent):
    """Validates results and flags issues"""

    def __init__(self):
        super().__init__("Validator", "validator", "!val001")
        self.validations = 0

    def validate(self, result: str) -> tuple[bool, str]:
        """Simulate validating a result"""
        time.sleep(random.uniform(0.2, 0.8))
        # 80% chance of valid
        is_valid = random.random() > 0.2
        if is_valid:
            return True, "Validation passed: data integrity confirmed"
        else:
            return False, "Validation WARNING: potential data inconsistency"

    def run_cycle(self):
        """Run one validation cycle"""
        messages = self.read_new_messages()

        # Validate ANALYSIS messages
        for msg in messages:
            body = msg.get("body", "")
            if "ANALYSIS:" in body and "[Validator]" not in body:
                is_valid, result = self.validate(body)
                status = "VALIDATED" if is_valid else "FLAGGED"
                self.post(f"{status}: {result}", reply_to=msg["id"])
                self.validations += 1
                return 1
        return 0


class ReporterAgent(BBSAgent):
    """Summarizes and creates final reports"""

    def __init__(self):
        super().__init__("Reporter", "reporter", "!rep001")
        self.reports_created = 0

    def create_report(self, findings: List[str]) -> str:
        """Create a summary report"""
        time.sleep(random.uniform(0.5, 1.0))
        return f"REPORT: Summary of {len(findings)} validated findings. All tasks completed successfully."

    def run_cycle(self, create_final: bool = False):
        """Run one reporting cycle"""
        messages = self.read_new_messages()

        if create_final:
            # Create final summary
            validated = [m for m in messages if "VALIDATED:" in m.get("body", "")]
            if validated:
                report = self.create_report(validated)
                self.post(report)
                self.reports_created += 1
                return 1
        return 0


def run_multi_agent_test(duration_seconds: int = 60, task_interval: float = 5.0):
    """Run the multi-agent communication test"""

    print("=" * 70)
    print("MULTI-AGENT BBS COMMUNICATION TEST")
    print("=" * 70)
    print(f"BBS Server: {BBS_URL}")
    print(f"Board: {BOARD}")
    print(f"Duration: {duration_seconds} seconds")
    print(f"Task interval: {task_interval} seconds")
    print()

    # Initialize agents
    coordinator = CoordinatorAgent()
    researcher = ResearcherAgent()
    analyst = AnalystAgent()
    validator = ValidatorAgent()
    reporter = ReporterAgent()

    agents = [coordinator, researcher, analyst, validator, reporter]

    # Initial announcement
    print("Agents announcing presence...")
    for agent in agents:
        agent.post(f"Agent {agent.name} ({agent.role}) is online and ready")
        time.sleep(0.3)

    print()
    print("-" * 70)
    print("Starting agent collaboration loop...")
    print("-" * 70)

    start_time = time.time()
    task_count = 0

    while time.time() - start_time < duration_seconds:
        elapsed = time.time() - start_time

        # Coordinator assigns tasks periodically
        if int(elapsed) % int(task_interval) == 0 and elapsed > 1:
            task_count += 1
            tasks = [
                ("Researcher", "Research current Solana network metrics"),
                ("Analyst", "Analyze recent transaction patterns"),
                ("Researcher", "Investigate top wallet activity"),
                ("Analyst", "Review DEX liquidity trends"),
            ]
            assignee, task = random.choice(tasks)
            print(f"\n[{elapsed:.1f}s] Coordinator assigning task #{task_count} to {assignee}")
            coordinator.assign_task(task, assignee)
            time.sleep(1)  # Prevent duplicate assignments

        # Each agent runs its cycle
        for agent in [researcher, analyst, validator]:
            result = agent.run_cycle()
            if result > 0:
                print(f"  [{elapsed:.1f}s] {agent.name} completed action")

        # Small delay between cycles
        time.sleep(0.5)

    # Final report
    print()
    print("-" * 70)
    print("Creating final report...")
    reporter.run_cycle(create_final=True)

    # Summary
    print()
    print("=" * 70)
    print("TEST COMPLETE - SUMMARY")
    print("=" * 70)
    print()
    print("Agent Statistics:")
    print("-" * 40)
    for agent in agents:
        print(f"  {agent.name:15} | Sent: {agent.messages_sent:3} | Read: {agent.messages_read:3}")
    print("-" * 40)
    total_sent = sum(a.messages_sent for a in agents)
    total_read = sum(a.messages_read for a in agents)
    print(f"  {'TOTAL':15} | Sent: {total_sent:3} | Read: {total_read:3}")
    print()
    print(f"Coordinator: {coordinator.tasks_assigned} tasks assigned, {coordinator.tasks_completed} completed")
    print(f"Validator: {validator.validations} validations performed")
    print(f"Reporter: {reporter.reports_created} reports created")
    print()

    # Verify messages on BBS
    print("Verifying messages on BBS...")
    resp = requests.get(f"{BBS_URL}/api/boards/{BOARD}/posts?limit=100")
    if resp.status_code == 200:
        messages = resp.json().get("data", [])
        print(f"  Total messages on {BOARD} board: {len(messages)}")

        # Count by agent
        agent_counts = {}
        for msg in messages:
            body = msg.get("body", "")
            for agent in agents:
                if f"[{agent.name}]" in body:
                    agent_counts[agent.name] = agent_counts.get(agent.name, 0) + 1

        print("  Messages by agent on server:")
        for name, count in sorted(agent_counts.items()):
            print(f"    {name}: {count}")

    print()
    print("=" * 70)
    return total_sent, total_read


if __name__ == "__main__":
    import sys

    duration = int(sys.argv[1]) if len(sys.argv) > 1 else 60
    run_multi_agent_test(duration_seconds=duration)
