# OSVM CLI Installer for Windows
# This script installs the OSVM CLI tool for managing Solana Virtual Machines

Write-Host "OSVM CLI Installer" -ForegroundColor Blue
Write-Host "==============================" -ForegroundColor Blue

# Check if Rust is installed
if (-not (Get-Command rustc -ErrorAction SilentlyContinue)) {
    Write-Host "Rust is not installed. Installing Rust..." -ForegroundColor Yellow
    Invoke-WebRequest -Uri 'https://static.rust-lang.org/rustup/dist/x86_64-pc-windows-msvc/rustup-init.exe' -OutFile 'rustup-init.exe'
    .\rustup-init.exe -y
    Remove-Item 'rustup-init.exe'
    $env:Path = [System.Environment]::GetEnvironmentVariable("Path","User") + ";" + [System.Environment]::GetEnvironmentVariable("Path","Machine")
    Write-Host "Rust installed successfully!" -ForegroundColor Green
} else {
    Write-Host "Rust is already installed." -ForegroundColor Green
}

# Check if Cargo is installed
if (-not (Get-Command cargo -ErrorAction SilentlyContinue)) {
    Write-Host "Cargo is not available. Please ensure Rust is properly installed." -ForegroundColor Red
    exit 1
} else {
    Write-Host "Cargo is available." -ForegroundColor Green
}

# Check for Solana CLI tools
if (-not (Get-Command solana -ErrorAction SilentlyContinue)) {
    Write-Host "Solana CLI tools not found. It's recommended to install them." -ForegroundColor Yellow
    Write-Host "You can install Solana CLI tools from: https://docs.solana.com/cli/install-solana-cli-tools"
}

Write-Host "Installing OSVM CLI..." -ForegroundColor Blue

# Install from crates.io
cargo install osvm

# Check if installation was successful
if (Get-Command osvm -ErrorAction SilentlyContinue) {
    Write-Host "OSVM CLI installed successfully!" -ForegroundColor Green
    Write-Host ""
    Write-Host "You can now use the OSVM CLI with the 'osvm' command."
    Write-Host "Try 'osvm --help' to get started."
} else {
    # If not in PATH, try installing from the repository
    Write-Host "Installing from GitHub repository..." -ForegroundColor Yellow
    
    # Create a temporary directory
    $TMP_DIR = New-TemporaryFile | ForEach-Object { Remove-Item $_; New-Item -ItemType Directory -Path $_.FullName }
    Push-Location $TMP_DIR
    
    # Clone the repository
    git clone https://github.com/opensvm/osvm-cli.git
    Set-Location osvm-cli
    
    # Build and install
    cargo build --release
    
    # Copy binary to install location
    $INSTALL_DIR = "$env:USERPROFILE\.cargo\bin"
    if (-not (Test-Path $INSTALL_DIR)) {
        New-Item -ItemType Directory -Path $INSTALL_DIR -Force | Out-Null
    }
    
    Copy-Item "target\release\osvm.exe" -Destination "$INSTALL_DIR\osvm.exe"
    
    # Add to PATH if not already there
    $userPath = [System.Environment]::GetEnvironmentVariable("Path", "User")
    if ($userPath -notlike "*$INSTALL_DIR*") {
        [System.Environment]::SetEnvironmentVariable("Path", $userPath + ";$INSTALL_DIR", "User")
        $env:Path = [System.Environment]::GetEnvironmentVariable("Path", "User") + ";" + [System.Environment]::GetEnvironmentVariable("Path", "Machine")
    }
    
    # Clean up
    Pop-Location
    Remove-Item -Recurse -Force $TMP_DIR
    
    # Final check
    if (Get-Command osvm -ErrorAction SilentlyContinue) {
        Write-Host "OSVM CLI installed successfully!" -ForegroundColor Green
        Write-Host ""
        Write-Host "You can now use the OSVM CLI with the 'osvm' command."
        Write-Host "Try 'osvm --help' to get started."
    } else {
        Write-Host "Installation failed. Please try installing manually:" -ForegroundColor Red
        Write-Host "1. Clone the repository: git clone https://github.com/opensvm/osvm-cli.git"
        Write-Host "2. Build the project: cd osvm-cli && cargo build --release"
        Write-Host "3. Copy the binary: Copy-Item 'target\release\osvm.exe' -Destination '$env:USERPROFILE\.cargo\bin\osvm.exe'"
        Write-Host "4. Ensure '$env:USERPROFILE\.cargo\bin' is in your PATH"
    }
}