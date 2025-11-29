/**
 * AEA Protocol - Quick Devnet Initialization Test
 * Run with: node test_init.mjs
 */

import {
  Connection,
  Keypair,
  PublicKey,
  Transaction,
  TransactionInstruction,
  sendAndConfirmTransaction,
  SystemProgram,
  LAMPORTS_PER_SOL
} from '@solana/web3.js';
import fs from 'fs';

const AEA_PROTOCOL = new PublicKey('8rHM6od1gkE9LkqnuDwLnrYMMGHqBNqsL1oGXh2QYASj');
const CONFIG_SIZE = 128;
const PARTICIPANT_SIZE = 256;

async function main() {
  console.log('=== AEA Protocol Devnet Test ===\n');

  const connection = new Connection('https://api.devnet.solana.com', 'confirmed');

  // Load payer keypair
  const keypairPath = '/tmp/devnet-deploy-1764245600.json';
  const keypairData = JSON.parse(fs.readFileSync(keypairPath, 'utf-8'));
  const payer = Keypair.fromSecretKey(Uint8Array.from(keypairData));

  const balance = await connection.getBalance(payer.publicKey);
  console.log(`Payer: ${payer.publicKey.toBase58()}`);
  console.log(`Balance: ${balance / LAMPORTS_PER_SOL} SOL\n`);

  // === Test 1: Create and Initialize Config Account ===
  console.log('Test 1: Initialize Protocol');
  console.log('-'.repeat(40));

  const configAccount = Keypair.generate();
  const configRent = await connection.getMinimumBalanceForRentExemption(CONFIG_SIZE);

  console.log(`Config account: ${configAccount.publicKey.toBase58()}`);
  console.log(`Rent required: ${configRent / LAMPORTS_PER_SOL} SOL`);

  // Create account instruction
  const createConfigIx = SystemProgram.createAccount({
    fromPubkey: payer.publicKey,
    newAccountPubkey: configAccount.publicKey,
    lamports: configRent,
    space: CONFIG_SIZE,
    programId: AEA_PROTOCOL,
  });

  // Initialize protocol instruction (discriminator 0)
  const initData = Buffer.alloc(9);
  initData.writeUInt8(0, 0); // discriminator
  initData.writeBigUInt64LE(BigInt(LAMPORTS_PER_SOL), 1); // min_agent_stake = 1 SOL

  const initIx = new TransactionInstruction({
    programId: AEA_PROTOCOL,
    keys: [
      { pubkey: configAccount.publicKey, isSigner: false, isWritable: true },
      { pubkey: payer.publicKey, isSigner: true, isWritable: false },
    ],
    data: initData,
  });

  const tx1 = new Transaction().add(createConfigIx, initIx);

  try {
    const sig1 = await sendAndConfirmTransaction(connection, tx1, [payer, configAccount]);
    console.log(`✅ Protocol initialized!`);
    console.log(`   Signature: ${sig1}`);
    console.log(`   Explorer: https://explorer.solana.com/tx/${sig1}?cluster=devnet`);

    // Verify config
    const configData = await connection.getAccountInfo(configAccount.publicKey);
    if (configData) {
      const initialized = configData.data[0];
      console.log(`   Initialized byte: ${initialized}`);
    }
  } catch (err) {
    console.log(`❌ Init failed: ${err.message}`);
    if (err.logs) {
      console.log('Logs:', err.logs.slice(-5).join('\n'));
    }
  }

  // === Test 2: Register User ===
  console.log('\nTest 2: Register User');
  console.log('-'.repeat(40));

  const userAccount = Keypair.generate();
  const userRent = await connection.getMinimumBalanceForRentExemption(PARTICIPANT_SIZE);

  console.log(`User account: ${userAccount.publicKey.toBase58()}`);

  const createUserIx = SystemProgram.createAccount({
    fromPubkey: payer.publicKey,
    newAccountPubkey: userAccount.publicKey,
    lamports: userRent,
    space: PARTICIPANT_SIZE,
    programId: AEA_PROTOCOL,
  });

  // Register user instruction (discriminator 10)
  // Data: disc(1) + endpoint(64) + display_name(32) = 97 bytes
  const registerData = Buffer.alloc(97);
  registerData.writeUInt8(10, 0); // discriminator
  Buffer.from('http://localhost:8080').copy(registerData, 1);
  Buffer.from('TestUser').copy(registerData, 65);

  const registerIx = new TransactionInstruction({
    programId: AEA_PROTOCOL,
    keys: [
      { pubkey: configAccount.publicKey, isSigner: false, isWritable: true },
      { pubkey: userAccount.publicKey, isSigner: false, isWritable: true },
      { pubkey: payer.publicKey, isSigner: true, isWritable: false },
    ],
    data: registerData,
  });

  const tx2 = new Transaction().add(createUserIx, registerIx);

  try {
    const sig2 = await sendAndConfirmTransaction(connection, tx2, [payer, userAccount]);
    console.log(`✅ User registered!`);
    console.log(`   Signature: ${sig2}`);
    console.log(`   Explorer: https://explorer.solana.com/tx/${sig2}?cluster=devnet`);

    // Verify user
    const userData = await connection.getAccountInfo(userAccount.publicKey);
    if (userData) {
      const participantType = userData.data[0];
      const status = userData.data[1];
      console.log(`   Type: ${participantType} (0=User, 1=Agent, 2=Provider)`);
      console.log(`   Status: ${status} (0=Inactive, 1=Active)`);
    }
  } catch (err) {
    console.log(`❌ Register failed: ${err.message}`);
    if (err.logs) {
      console.log('Logs:', err.logs.slice(-5).join('\n'));
    }
  }

  // === Summary ===
  console.log('\n' + '='.repeat(40));
  console.log('Summary');
  console.log('='.repeat(40));
  console.log(`Config Account:  ${configAccount.publicKey.toBase58()}`);
  console.log(`User Account:    ${userAccount.publicKey.toBase58()}`);

  const finalBalance = await connection.getBalance(payer.publicKey);
  console.log(`\nFinal Balance: ${finalBalance / LAMPORTS_PER_SOL} SOL`);
  console.log(`Cost: ${(balance - finalBalance) / LAMPORTS_PER_SOL} SOL`);
}

main().catch(console.error);
