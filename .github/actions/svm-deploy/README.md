# SVM Deploy Action

This GitHub Action deploys Solana Virtual Machine (SVM) nodes using the openSVM CLI tool. It abstracts away the complexity of SVM deployments, making it easy to integrate SVM node deployment into your CI/CD pipelines.

## Features

- 🚀 **Simple Integration**: One-step SVM deployment with minimal configuration
- 🔒 **Secure**: Uses GitHub Secrets for sensitive data like SSH keys and Solana keypairs
- 🎯 **Configurable**: Support for different networks, node types, and deployment options
- 📊 **Detailed Logging**: Comprehensive logs for troubleshooting and monitoring
- 🔄 **Flexible**: Supports additional CLI arguments for advanced use cases

## Usage

### Basic Example

```yaml
name: Deploy SVM

on:
  push:
    branches: [ main ]

jobs:
  deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      
      - name: Deploy SVM Node
        uses: ./.github/actions/svm-deploy
        with:
          svm-name: 'my-svm'
          host: 'user@example.com'
          ssh-private-key: ${{ secrets.SSH_PRIVATE_KEY }}
          network: 'devnet'
          node-type: 'validator'
```

### Advanced Example

```yaml
name: Deploy SVM with Custom Configuration

on:
  workflow_dispatch:
    inputs:
      svm_name:
        description: 'SVM name to deploy'
        required: true
        default: 'my-svm'
      target_host:
        description: 'Target host for deployment'
        required: true
        default: 'user@example.com'
      network:
        description: 'Network to deploy on'
        required: true
        default: 'devnet'
        type: choice
        options:
          - mainnet
          - testnet
          - devnet

jobs:
  deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      
      - name: Deploy SVM Node
        uses: ./.github/actions/svm-deploy
        with:
          svm-name: ${{ github.event.inputs.svm_name }}
          host: ${{ github.event.inputs.target_host }}
          network: ${{ github.event.inputs.network }}
          node-type: 'validator'
          ssh-private-key: ${{ secrets.SSH_PRIVATE_KEY }}
          keypair: ${{ secrets.SOLANA_KEYPAIR }}
          rpc-url: ${{ secrets.RPC_URL }}
          verbose: true
          deploy-args: '--custom-flag value'
```

## Inputs

| Input | Description | Required | Default |
|-------|-------------|----------|---------|
| `svm-name` | Name of the SVM to deploy | ✅ | - |
| `host` | Remote host to deploy to (format: user@host[:port]) | ✅ | - |
| `network` | Target Solana network (mainnet, testnet, devnet) | ❌ | `mainnet` |
| `node-type` | Type of node to deploy (validator, rpc) | ❌ | `validator` |
| `ssh-private-key` | SSH private key for connecting to remote host | ✅ | - |
| `keypair` | Solana keypair JSON content | ❌ | - |
| `rpc-url` | JSON RPC URL for the cluster | ❌ | - |
| `verbose` | Enable verbose logging | ❌ | `false` |
| `deploy-args` | Additional CLI flags for deployment | ❌ | - |

## Outputs

| Output | Description |
|--------|-------------|
| `status` | Deployment status (success or failure) |
| `logs` | Complete deployment logs |

## Secrets Setup

### Required Secrets

1. **SSH_PRIVATE_KEY**: Private SSH key for connecting to your remote host
   ```bash
   # Generate a new SSH key pair
   ssh-keygen -t ed25519 -C "github-actions" -f ~/.ssh/github_actions
   
   # Add the private key to GitHub Secrets as SSH_PRIVATE_KEY
   cat ~/.ssh/github_actions
   
   # Add the public key to your remote host's authorized_keys
   cat ~/.ssh/github_actions.pub >> ~/.ssh/authorized_keys
   ```

### Optional Secrets

2. **SOLANA_KEYPAIR**: Solana keypair JSON content
   ```bash
   # Generate a new Solana keypair
   solana-keygen new --no-bip39-passphrase -o keypair.json
   
   # Add the keypair content to GitHub Secrets as SOLANA_KEYPAIR
   cat keypair.json
   ```

3. **RPC_URL**: Custom RPC endpoint URL
   ```
   # Example RPC URLs
   https://api.mainnet-beta.solana.com
   https://api.devnet.solana.com
   https://api.testnet.solana.com
   ```

## Security Considerations

- **Never commit private keys or secrets** to your repository
- Use GitHub Secrets for all sensitive data
- Ensure your SSH private key has appropriate permissions (600)
- Consider using dedicated keypairs for CI/CD deployments
- Regularly rotate your SSH keys and Solana keypairs

## Troubleshooting

### Common Issues

1. **SSH Connection Failed**
   - Verify the SSH private key is correct
   - Check that the public key is added to the remote host's authorized_keys
   - Ensure the host format is correct (user@host[:port])

2. **SVM Installation Failed**
   - Check the verbose logs for detailed error messages
   - Verify the SVM name is correct and available
   - Ensure the remote host meets system requirements

3. **Network Issues**
   - Verify the network parameter is correct (mainnet, testnet, devnet)
   - Check if the specified RPC URL is accessible
   - Ensure firewall rules allow the necessary connections

### Getting Help

If you encounter issues:

1. Enable verbose logging by setting `verbose: true`
2. Check the action logs for detailed error messages
3. Verify your secrets are correctly configured
4. Consult the [openSVM CLI documentation](https://github.com/openSVM/osvm-cli)

## Examples Repository

For more examples and use cases, check out the [examples directory](../../../examples/) in the main repository.

## Contributing

Contributions are welcome! Please:

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests if applicable
5. Submit a pull request

## License

This action is licensed under the MIT License. See the [LICENSE](../../../LICENSE) file for details.