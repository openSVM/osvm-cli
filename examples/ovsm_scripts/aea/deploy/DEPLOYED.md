# AEA Protocol - Devnet Deployment

**Deployment Date:** 2025-11-29

## Program IDs (Devnet)

| Program | Program ID | Size |
|---------|-----------|------|
| **AEA Protocol** | `8rHM6od1gkE9LkqnuDwLnrYMMGHqBNqsL1oGXh2QYASj` | 27 KB |
| **AEA Negotiation** | `CfkaPnSDUswrHAie1D2pahU4yBbuGRjioS4i5efVy2KX` | 19 KB |
| **AEA Governance** | `CFqhR4CxwWWUTpPSrVEJADee3pGJu7suYU1mLpcseDqZ` | 18 KB |

## Verification

```bash
# Verify programs are deployed
solana program show 8rHM6od1gkE9LkqnuDwLnrYMMGHqBNqsL1oGXh2QYASj --url devnet
solana program show CfkaPnSDUswrHAie1D2pahU4yBbuGRjioS4i5efVy2KX --url devnet
solana program show CFqhR4CxwWWUTpPSrVEJADee3pGJu7suYU1mLpcseDqZ --url devnet
```

## Explorer Links

- [AEA Protocol](https://explorer.solana.com/address/8rHM6od1gkE9LkqnuDwLnrYMMGHqBNqsL1oGXh2QYASj?cluster=devnet)
- [AEA Negotiation](https://explorer.solana.com/address/CfkaPnSDUswrHAie1D2pahU4yBbuGRjioS4i5efVy2KX?cluster=devnet)
- [AEA Governance](https://explorer.solana.com/address/CFqhR4CxwWWUTpPSrVEJADee3pGJu7suYU1mLpcseDqZ?cluster=devnet)

## Usage

### Initialize Protocol
```bash
# Create config account PDA
solana program invoke 8rHM6od1gkE9LkqnuDwLnrYMMGHqBNqsL1oGXh2QYASj \
  --data 00 \
  --url devnet
```

### Register as User (discriminator 10)
### Register as Agent (discriminator 11)
### Create Order (discriminator 40)
See IDL files for full instruction set.
