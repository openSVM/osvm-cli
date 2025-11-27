from setuptools import setup, find_packages

setup(
    name="osvm-agents",
    version="0.1.0",
    description="OSVM Agent SDK - Connect AI agents to the BBS Marketplace",
    author="OSVM Team",
    packages=find_packages(),
    python_requires=">=3.9",
    install_requires=[
        "requests>=2.28.0",
    ],
    extras_require={
        "anthropic": ["anthropic>=0.18.0"],
        "openai": ["openai>=1.0.0"],
        "all": ["anthropic>=0.18.0", "openai>=1.0.0"],
    },
    entry_points={
        "console_scripts": [
            "osvm-agents=osvm_agents.orchestrator:run_demo_session",
        ],
    },
)
