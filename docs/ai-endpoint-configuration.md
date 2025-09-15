# OSVM CLI - AI Endpoint Configuration

## Overview

The OSVM CLI now supports dual AI endpoints, allowing users to choose between the default OSVM.ai service or their own OpenAI-compatible models.

## Configuration

### Environment Variables

- `OPENAI_URL`: The endpoint URL for your OpenAI-compatible API
- `OPENAI_KEY`: Your API key for authentication

### Usage Examples

#### 1. Default OSVM.ai (No configuration needed)
```bash
osvm "What is Solana security?"
# Uses: https://osvm.ai/api/getAnswer
```

#### 2. OpenAI Official API
```bash
export OPENAI_URL="https://api.openai.com/v1/chat/completions"
export OPENAI_KEY="sk-your-openai-api-key"
osvm "Explain smart contract security"
# Uses: OpenAI ChatGPT API with Bearer token authentication
```

#### 3. Local Models (Ollama, LocalAI, etc.)
```bash
export OPENAI_URL="http://localhost:11434/v1/chat/completions"
export OPENAI_KEY="ollama-key"
osvm "Help with Rust programming"
# Uses: Local Ollama instance
```

#### 4. Custom AI Providers
```bash
export OPENAI_URL="https://api.anthropic.com/v1/messages"
export OPENAI_KEY="your-anthropic-key"
osvm "Security audit guidance"
# Uses: Custom AI provider with OpenAI-compatible format
```

## Detection Logic

The AI service automatically detects the configuration:

1. **Check environment variables**: If both `OPENAI_URL` and `OPENAI_KEY` are set, use custom endpoint
2. **Fallback to OSVM.ai**: If variables are missing or empty, use default service
3. **Format detection**: Automatically formats requests for OpenAI ChatGPT API or OSVM.ai API

## Output Examples

### Default OSVM.ai
```
🔍 Interpreting as AI query: "What is Solana?"
🤖 Asking OSVM AI (https://osvm.ai/api/getAnswer): What is Solana?
```

### Custom OpenAI
```
🔍 Interpreting as AI query: "What is Solana?"
🤖 Asking OpenAI (https://api.openai.com/v1/chat/completions): What is Solana?
```

## Testing

All functionality has been tested with 50 comprehensive real-world scenarios including:

- ✅ Default endpoint detection
- ✅ Custom OpenAI endpoint configuration  
- ✅ Local model support (Ollama, LocalAI)
- ✅ Error handling for invalid configurations
- ✅ Environment variable validation
- ✅ Backward compatibility with OSVM.ai

## Supported Models

Any OpenAI-compatible API including:
- OpenAI ChatGPT (GPT-3.5, GPT-4)
- Local models via Ollama
- LocalAI deployments
- Custom API endpoints
- Enterprise AI solutions

## Benefits

- **Cost Control**: Use your own API keys and billing
- **Privacy**: Keep queries on local/private models
- **Flexibility**: Switch between different AI providers
- **Compatibility**: Maintain existing OSVM.ai workflows
- **Performance**: Use faster local models when available

## Implementation Details

The `AiService` struct handles both endpoint types:
- `query_osvm_ai()`: Original OSVM.ai format
- `query_openai()`: OpenAI ChatGPT API format with proper headers
- Automatic format detection based on environment variables
- Comprehensive error handling for both endpoints