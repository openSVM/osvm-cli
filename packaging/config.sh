#!/bin/bash
# Configuration for OSVM CLI packaging
# This centralizes maintainer information and other packaging constants

# Maintainer information (can be overridden by environment variables)
OSVM_MAINTAINER_NAME="${OSVM_MAINTAINER_NAME:-OpenSVM}"
OSVM_MAINTAINER_EMAIL="${OSVM_MAINTAINER_EMAIL:-rin@opensvm.com}"

# Project information
OSVM_PROJECT_URL="${OSVM_PROJECT_URL:-https://github.com/openSVM/osvm-cli}"
OSVM_DESCRIPTION_SHORT="OpenSVM CLI tool for managing SVM nodes and deployments"
OSVM_DESCRIPTION_LONG="OSVM CLI is a comprehensive tool for managing Solana Virtual Machine (SVM) nodes and deployments. It provides functionality for node deployment, SVM blockchain interactions, audit capabilities, and SSH-based deployment automation."

# Export variables for use in scripts
export OSVM_MAINTAINER_NAME
export OSVM_MAINTAINER_EMAIL
export OSVM_PROJECT_URL
export OSVM_DESCRIPTION_SHORT
export OSVM_DESCRIPTION_LONG