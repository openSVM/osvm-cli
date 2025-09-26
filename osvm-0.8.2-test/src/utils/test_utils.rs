use solana_sdk::signature::{Keypair, Signer};
use std::io::Write;
use std::fs::File;
use std::path::Path;

pub fn create_test_keypair_file(path: &Path) -> std::io::Result<()> {
    let keypair = Keypair::new();
    let mut file = File::create(path)?;
    let json = serde_json::to_string(&keypair.to_bytes().to_vec()).unwrap();
    file.write_all(json.as_bytes())?;
    Ok(())
}
