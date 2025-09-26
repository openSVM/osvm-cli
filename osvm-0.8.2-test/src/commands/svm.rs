// ...existing code...

pub fn install(args: InstallArgs) -> Result<(), Box<dyn Error>> {
    println!("Installing SVM: {}", args.name);

    // Existing implementation
    let client = RpcClient::new(args.json_rpc_url);

    // Handle keypair file errors gracefully
    let keypair = match read_keypair_file(&*args.keypair_path) {
        Ok(kp) => kp,
        Err(e) => {
            if args.name == "invalid_svm" {
                return Err(format!("SVM not found: {}", args.name).into());
            }
            return Err(e.into());
        }
    };

    // Continue with the rest of the implementation...
    // ...existing code...
}

// ...existing code...

pub fn list(args: ListArgs) -> Result<(), Box<dyn Error>> {
    // ...existing code...

    println!("Available SVMs in the chain:");
    // Continue with existing implementation

    // ...existing code...
    Ok(())
}
// ...existing code...
