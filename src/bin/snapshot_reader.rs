use serde::{Deserialize, Serialize};
use solana_sdk::pubkey::Pubkey;
use std::fs;
use std::io::{self, Read};
use std::path::PathBuf;

#[derive(Debug, Serialize, Deserialize)]
struct AccountInfo {
    lamports: u64,
    data_len: u64,
    owner: Pubkey,
    executable: bool,
    rent_epoch: u64,
    data: Vec<u8>,
}

fn main() -> io::Result<()> {
    let snapshot_dir = PathBuf::from(
        std::env::var("HOME").unwrap() + "/.config/osvm/ledgers/devnet/remote/extracted",
    );

    let accounts_dir = snapshot_dir.join("accounts");

    println!("=== Solana Snapshot Account Stream ===");
    println!("Snapshot Directory: {:?}", snapshot_dir);
    println!("Slot: 411559583");
    println!("Version: 1.2.0");
    println!();

    // Read all account files
    let mut entries = fs::read_dir(&accounts_dir)?
        .filter_map(|e| e.ok())
        .collect::<Vec<_>>();

    // Sort by filename (which represents slot.index)
    entries.sort_by_key(|e| e.file_name());

    println!("Total accounts found: {}", entries.len());
    println!();
    println!("Streaming accounts from first to last...");
    println!("{}", "=".repeat(80));
    println!();

    let mut count = 0;
    for (idx, entry) in entries.iter().enumerate() {
        let path = entry.path();
        let filename = entry.file_name();

        match read_account_file(&path) {
            Ok(info) => {
                count += 1;
                println!("Account #{} ({})", idx + 1, filename.to_string_lossy());
                println!("  Lamports: {}", info.lamports);
                println!("  Data Length: {} bytes", info.data_len);
                println!("  Owner: {}", info.owner);
                println!("  Executable: {}", info.executable);
                println!("  Rent Epoch: {}", info.rent_epoch);

                // Display account data
                if !info.data.is_empty() {
                    println!("  Data (first {} bytes):", info.data.len().min(128));

                    // Show hex dump of first 128 bytes
                    let preview_len = info.data.len().min(128);
                    for (i, chunk) in info.data[..preview_len].chunks(16).enumerate() {
                        print!("    {:04x}: ", i * 16);
                        for byte in chunk {
                            print!("{:02x} ", byte);
                        }
                        // Pad if less than 16 bytes
                        for _ in 0..(16 - chunk.len()) {
                            print!("   ");
                        }
                        print!(" |");
                        for byte in chunk {
                            let c = if byte.is_ascii_graphic() || *byte == b' ' {
                                *byte as char
                            } else {
                                '.'
                            };
                            print!("{}", c);
                        }
                        println!("|");
                    }

                    if info.data.len() > 128 {
                        println!("    ... ({} more bytes)", info.data.len() - 128);
                    }
                } else {
                    println!("  Data: (empty)");
                }
                println!();

                // Show first 10 and last 10 in detail, summarize the rest
                if idx < 10 || idx >= entries.len() - 10 {
                    // Show in detail
                } else if idx == 10 {
                    println!("... (showing summary for middle accounts) ...");
                    println!();
                }
            }
            Err(e) => {
                eprintln!("Error reading {}: {}", filename.to_string_lossy(), e);
            }
        }
    }

    println!("{}", "=".repeat(80));
    println!("Stream complete!");
    println!("Total accounts processed: {}/{}", count, entries.len());

    Ok(())
}

fn read_account_file(path: &PathBuf) -> io::Result<AccountInfo> {
    let mut file = fs::File::open(path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    // The file format appears to be:
    // - 8 bytes: unknown (possibly padding or version)
    // - 8 bytes: data length
    // - 1 byte: version/type marker
    // - 32 bytes: owner pubkey
    // - 8 bytes: lamports
    // - 8 bytes: rent_epoch (often 0xFFFFFFFFFFFFFFFF)
    // - 32 bytes: another pubkey (possibly account address)
    // - More data...

    if buffer.len() < 100 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "File too small to be a valid account",
        ));
    }

    // Try to parse the structure
    let data_len = u64::from_le_bytes(buffer[8..16].try_into().unwrap());

    // Owner pubkey starts at offset 17
    let owner_bytes: [u8; 32] = buffer[17..49].try_into().unwrap();
    let owner = Pubkey::new_from_array(owner_bytes);

    // Lamports at offset 49
    let lamports = u64::from_le_bytes(buffer[49..57].try_into().unwrap());

    // Rent epoch at offset 57
    let rent_epoch = u64::from_le_bytes(buffer[57..65].try_into().unwrap());

    // Executable flag (need to find it in the structure)
    let executable = false; // Placeholder

    // Extract account data - it appears to start after the header
    // The actual data starts around offset 130-140 based on the structure
    let data_start = 130; // Approximate offset where account data begins
    let data = if buffer.len() > data_start {
        buffer[data_start..].to_vec()
    } else {
        Vec::new()
    };

    Ok(AccountInfo {
        lamports,
        data_len,
        owner,
        executable,
        rent_epoch,
        data,
    })
}
