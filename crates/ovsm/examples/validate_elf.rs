//! Validate ELF with solana_rbpf
use solana_rbpf::{
    elf::Executable,
    program::{BuiltinProgram, SBPFVersion},
    verifier::RequisiteVerifier,
};

#[derive(Debug)]
struct TestContext {
    remaining: u64,
}

impl solana_rbpf::vm::ContextObject for TestContext {
    fn trace(&mut self, _state: [u64; 12]) {}
    fn consume(&mut self, amount: u64) {
        self.remaining = self.remaining.saturating_sub(amount);
    }
    fn get_remaining(&self) -> u64 {
        self.remaining
    }
}

fn main() {
    let elf_bytes = std::fs::read("/tmp/amm_program.so").expect("read elf");
    println!("ELF size: {} bytes", elf_bytes.len());

    let loader = std::sync::Arc::new(BuiltinProgram::<TestContext>::new_mock());

    match Executable::<TestContext>::from_elf(&elf_bytes, loader) {
        Ok(exe) => {
            println!("✅ Valid sBPF ELF (loaded)!");
            println!(
                "   Entry offset: {}",
                exe.get_entrypoint_instruction_offset()
            );

            // Verify bytecode
            match exe.verify::<RequisiteVerifier>() {
                Ok(()) => println!("   Verification: ✅ PASS"),
                Err(e) => println!("   Verification: ❌ {:?}", e),
            }
        }
        Err(e) => {
            println!("❌ Invalid ELF: {:?}", e);
        }
    }
}
