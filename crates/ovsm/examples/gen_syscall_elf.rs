// Compile syscall test and regenerate ELF
use ovsm::compiler::{CompileOptions, Compiler};

fn main() {
    let source = r#";; Test syscall
(do
  (syscall "sol_log_" "hello opensvm from $ovsm")
  42)
"#;

    let opts = CompileOptions {
        opt_level: 0,
        ..Default::default()
    };

    let mut compiler = Compiler::new(opts);
    match compiler.compile(source) {
        Ok(result) => {
            std::fs::write("/tmp/hello_final.so", &result.elf_bytes).expect("Failed to write ELF");
            println!(
                "✅ Generated /tmp/hello_final.so ({} bytes)",
                result.elf_bytes.len()
            );
        }
        Err(e) => {
            eprintln!("❌ Compilation failed: {:?}", e);
            std::process::exit(1);
        }
    }
}
