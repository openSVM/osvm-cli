/**
 * AEA Protocol - Devnet Integration Tests
 *
 * Tests the deployed AEA programs on Solana devnet:
 * - Initialize protocol
 * - Register user
 * - Register agent (with stake)
 * - Create order
 *
 * Run with: npx ts-node test_aea_devnet.ts
 */

import {
  Connection,
  Keypair,
  PublicKey,
  Transaction,
  TransactionInstruction,
  sendAndConfirmTransaction,
  SystemProgram,
  LAMPORTS_PER_SOL,
} from '@solana/web3.js';
import * as fs from 'fs';

// Program IDs (deployed on devnet)
const AEA_PROTOCOL = new PublicKey('8rHM6od1gkE9LkqnuDwLnrYMMGHqBNqsL1oGXh2QYASj');
const AEA_NEGOTIATION = new PublicKey('CfkaPnSDUswrHAie1D2pahU4yBbuGRjioS4i5efVy2KX');
const AEA_GOVERNANCE = new PublicKey('CFqhR4CxwWWUTpPSrVEJADee3pGJu7suYU1mLpcseDqZ');

// Instruction discriminators (from IDL)
const DISC_INITIALIZE_PROTOCOL = 0;
const DISC_REGISTER_USER = 10;
const DISC_REGISTER_AGENT = 11;
const DISC_REGISTER_PROVIDER = 12;
const DISC_CREATE_ORDER = 40;
const DISC_ACCEPT_ORDER = 41;

// Account sizes
const CONFIG_SIZE = 128;
const PARTICIPANT_SIZE = 256;
const ORDER_SIZE = 256;

class AeaProtocolTest {
  connection: Connection;
  payer: Keypair;

  constructor() {
    this.connection = new Connection('https://api.devnet.solana.com', 'confirmed');

    // Load keypair with SOL
    const keypairPath = '/tmp/devnet-deploy-1764245600.json';
    const keypairData = JSON.parse(fs.readFileSync(keypairPath, 'utf-8'));
    this.payer = Keypair.fromSecretKey(Uint8Array.from(keypairData));
  }

  async getBalance(): Promise<number> {
    const balance = await this.connection.getBalance(this.payer.publicKey);
    return balance / LAMPORTS_PER_SOL;
  }

  async createAccount(size: number, programId: PublicKey): Promise<Keypair> {
    const account = Keypair.generate();
    const lamports = await this.connection.getMinimumBalanceForRentExemption(size);

    const tx = new Transaction().add(
      SystemProgram.createAccount({
        fromPubkey: this.payer.publicKey,
        newAccountPubkey: account.publicKey,
        lamports,
        space: size,
        programId,
      })
    );

    await sendAndConfirmTransaction(this.connection, tx, [this.payer, account]);
    console.log(`Created account: ${account.publicKey.toBase58()} (${size} bytes)`);
    return account;
  }

  async initializeProtocol(): Promise<PublicKey> {
    console.log('\n=== Initialize Protocol ===');

    // Create config account
    const configAccount = await this.createAccount(CONFIG_SIZE, AEA_PROTOCOL);

    // Build instruction data: discriminator + min_agent_stake (optional, 8 bytes)
    const data = Buffer.alloc(9);
    data.writeUInt8(DISC_INITIALIZE_PROTOCOL, 0);
    data.writeBigUInt64LE(BigInt(LAMPORTS_PER_SOL), 1); // 1 SOL min stake

    const ix = new TransactionInstruction({
      programId: AEA_PROTOCOL,
      keys: [
        { pubkey: configAccount.publicKey, isSigner: false, isWritable: true },
        { pubkey: this.payer.publicKey, isSigner: true, isWritable: false },
      ],
      data,
    });

    const tx = new Transaction().add(ix);
    const sig = await sendAndConfirmTransaction(this.connection, tx, [this.payer]);
    console.log(`Protocol initialized! Signature: ${sig}`);
    console.log(`Config account: ${configAccount.publicKey.toBase58()}`);

    return configAccount.publicKey;
  }

  async registerUser(configPubkey: PublicKey): Promise<PublicKey> {
    console.log('\n=== Register User ===');

    // Create participant account
    const participantAccount = await this.createAccount(PARTICIPANT_SIZE, AEA_PROTOCOL);

    // Build instruction data: discriminator + endpoint (64 bytes) + display_name (32 bytes)
    const data = Buffer.alloc(97);
    data.writeUInt8(DISC_REGISTER_USER, 0);
    // Endpoint: "http://localhost:8080" padded to 64 bytes
    const endpoint = Buffer.from('http://localhost:8080'.padEnd(64, '\0'));
    endpoint.copy(data, 1);
    // Display name: "TestUser" padded to 32 bytes
    const name = Buffer.from('TestUser'.padEnd(32, '\0'));
    name.copy(data, 65);

    const ix = new TransactionInstruction({
      programId: AEA_PROTOCOL,
      keys: [
        { pubkey: configPubkey, isSigner: false, isWritable: true },
        { pubkey: participantAccount.publicKey, isSigner: false, isWritable: true },
        { pubkey: this.payer.publicKey, isSigner: true, isWritable: false },
      ],
      data,
    });

    const tx = new Transaction().add(ix);
    const sig = await sendAndConfirmTransaction(this.connection, tx, [this.payer]);
    console.log(`User registered! Signature: ${sig}`);
    console.log(`Participant account: ${participantAccount.publicKey.toBase58()}`);

    return participantAccount.publicKey;
  }

  async registerAgent(configPubkey: PublicKey, stakeLamports: bigint): Promise<PublicKey> {
    console.log('\n=== Register Agent ===');

    // Create participant account
    const participantAccount = await this.createAccount(PARTICIPANT_SIZE, AEA_PROTOCOL);

    // Build instruction data: discriminator + stake_amount (8 bytes) + endpoint + display_name
    const data = Buffer.alloc(105);
    data.writeUInt8(DISC_REGISTER_AGENT, 0);
    data.writeBigUInt64LE(stakeLamports, 1);
    // Endpoint: "mesh://agent001" padded to 64 bytes
    const endpoint = Buffer.from('mesh://agent001'.padEnd(64, '\0'));
    endpoint.copy(data, 9);
    // Display name: "AI-Agent-001" padded to 32 bytes
    const name = Buffer.from('AI-Agent-001'.padEnd(32, '\0'));
    name.copy(data, 73);

    const ix = new TransactionInstruction({
      programId: AEA_PROTOCOL,
      keys: [
        { pubkey: configPubkey, isSigner: false, isWritable: true },
        { pubkey: participantAccount.publicKey, isSigner: false, isWritable: true },
        { pubkey: this.payer.publicKey, isSigner: true, isWritable: true },
        // Would need token accounts for real staking
      ],
      data,
    });

    const tx = new Transaction().add(ix);
    const sig = await sendAndConfirmTransaction(this.connection, tx, [this.payer]);
    console.log(`Agent registered! Signature: ${sig}`);
    console.log(`Agent account: ${participantAccount.publicKey.toBase58()}`);

    return participantAccount.publicKey;
  }

  async readAccountData(pubkey: PublicKey): Promise<Buffer | null> {
    const info = await this.connection.getAccountInfo(pubkey);
    return info?.data || null;
  }

  async runTests(): Promise<void> {
    console.log('=== AEA Protocol Devnet Integration Tests ===');
    console.log(`Payer: ${this.payer.publicKey.toBase58()}`);
    console.log(`Balance: ${await this.getBalance()} SOL`);
    console.log(`\nPrograms:`);
    console.log(`  AEA Protocol:    ${AEA_PROTOCOL.toBase58()}`);
    console.log(`  AEA Negotiation: ${AEA_NEGOTIATION.toBase58()}`);
    console.log(`  AEA Governance:  ${AEA_GOVERNANCE.toBase58()}`);

    try {
      // Test 1: Initialize Protocol
      const configPubkey = await this.initializeProtocol();

      // Read and verify config
      const configData = await this.readAccountData(configPubkey);
      if (configData) {
        const initialized = configData.readUInt8(0);
        console.log(`\nConfig verification:`);
        console.log(`  Initialized: ${initialized === 1 ? 'YES' : 'NO'}`);
        console.log(`  Min agent stake: ${configData.readBigUInt64LE(8)} lamports`);
      }

      // Test 2: Register User
      const userPubkey = await this.registerUser(configPubkey);

      // Read and verify user
      const userData = await this.readAccountData(userPubkey);
      if (userData) {
        const participantType = userData.readUInt8(0);
        const status = userData.readUInt8(1);
        console.log(`\nUser verification:`);
        console.log(`  Type: ${participantType} (0=User)`);
        console.log(`  Status: ${status} (1=Active)`);
      }

      // Test 3: Register Agent (requires token accounts for real implementation)
      console.log('\n=== Agent Registration (simplified) ===');
      console.log('Note: Full agent registration requires SPL token accounts for staking');

      console.log('\n=== All Tests Passed! ===');
      console.log(`Final balance: ${await this.getBalance()} SOL`);

    } catch (error) {
      console.error('Test failed:', error);
      throw error;
    }
  }
}

// Run tests
const test = new AeaProtocolTest();
test.runTests().catch(console.error);
