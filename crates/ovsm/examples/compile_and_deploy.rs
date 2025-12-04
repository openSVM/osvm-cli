//! Compile OVSM to sBPF and deploy to localnet
use ovsm::compiler::{debug_compile, CompileOptions, Compiler};
use std::process::Command;

fn main() {
    let source = std::fs::read_to_string("/tmp/hello_test.ovsm")
        .expect("Failed to read /tmp/hello_test.ovsm");

    println!("=== Compiling OVSM to sBPF ===\n");

    // Full debug output
    debug_compile(&source);

    // Compile to ELF
    let options = CompileOptions {
        opt_level: 0,
        debug_info: true,
        ..Default::default()
    };
    let compiler = Compiler::new(options);

    match compiler.compile(&source) {
        Ok(result) => {
            let elf_path = "/tmp/amm_program.so";
            std::fs::write(elf_path, &result.elf_bytes).expect("Failed to write ELF");
            println!("\n✅ Wrote ELF to {}", elf_path);
            println!("   Size: {} bytes", result.elf_bytes.len());

            // Generate program keypair
            let program_keypair = "/tmp/amm_program_keypair.json";
            let keygen = Command::new("solana-keygen")
                .args([
                    "new",
                    "--no-bip39-passphrase",
                    "--outfile",
                    program_keypair,
                    "--force",
                ])
                .output()
                .expect("Failed to generate keypair");

            if !keygen.status.success() {
                eprintln!("Failed to generate program keypair");
                return;
            }

            // Get program ID
            let pubkey_output = Command::new("solana-keygen")
                .args(["pubkey", program_keypair])
                .output()
                .expect("Failed to get pubkey");
            let program_id = String::from_utf8_lossy(&pubkey_output.stdout)
                .trim()
                .to_string();
            println!("   Program ID: {}", program_id);

            println!("\n=== Deploying to localnet ===\n");

            // Deploy
            let deploy = Command::new("solana")
                .args([
                    "program",
                    "deploy",
                    "--keypair",
                    "/tmp/test-deploy-keypair.json",
                    "--program-id",
                    program_keypair,
                    "--url",
                    "http://localhost:8899",
                    elf_path,
                ])
                .output()
                .expect("Failed to deploy");

            println!("Deploy stdout: {}", String::from_utf8_lossy(&deploy.stdout));
            println!("Deploy stderr: {}", String::from_utf8_lossy(&deploy.stderr));

            if deploy.status.success() {
                println!("\n🎉 Program deployed successfully!");
                println!("   Program ID: {}", program_id);

                // Verify deployment
                let account = Command::new("solana")
                    .args([
                        "program",
                        "show",
                        "--url",
                        "http://localhost:8899",
                        &program_id,
                    ])
                    .output();

                if let Ok(output) = account {
                    println!("\n=== Program Info ===");
                    println!("{}", String::from_utf8_lossy(&output.stdout));
                }
            } else {
                println!("\n❌ Deployment failed");
                println!("   This is expected - our minimal ELF may not pass full BPF loader verification");
            }
        }
        Err(e) => {
            eprintln!("❌ Compilation failed: {:?}", e);
        }
    }
}
