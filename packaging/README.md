# OSVM CLI Packaging

This directory contains packaging scripts and configuration for building OSVM CLI packages for different platforms.

## Configuration

All packaging scripts use centralized configuration from `config.sh`. This allows easy customization of maintainer information and other settings.

### Environment Variables

You can override packaging configuration using environment variables:

- `OSVM_MAINTAINER_NAME`: Package maintainer name (default: "OpenSVM")
- `OSVM_MAINTAINER_EMAIL`: Package maintainer email (default: "rin@opensvm.com")
- `OSVM_PROJECT_URL`: Project homepage URL
- `OSVM_DESCRIPTION_SHORT`: Short package description
- `OSVM_DESCRIPTION_LONG`: Detailed package description

## Available Packaging Formats

### Debian/Ubuntu (.deb)
Location: `packaging/debian/`

Build locally:
```bash
cd packaging/debian
./build-deb.sh [version]
```

The script will:
- Create a source tarball
- Set up debian packaging structure with dh_make
- Configure proper maintainer information (OpenSVM)
- Build the .deb package with correct dependencies

### ArchLinux (PKGBUILD)
Location: `packaging/archlinux/`

Build locally:
```bash
cd packaging/archlinux
./build-arch.sh [version]
```

The script will:
- Update PKGBUILD with current version
- Generate .SRCINFO file for AUR submission
- Calculate proper SHA256 checksums

To build the package:
```bash
cd packaging/archlinux
makepkg -sri
```

### Termux (.deb for Android)
Location: `packaging/termux/`

Build locally:
```bash
cd packaging/termux
./build-termux.sh [version] [architecture]
```

The script will:
- Build a Termux-compatible .deb package
- Set up proper Termux directory structure (`/data/data/com.termux/files/usr/`)
- Include install/remove scripts with proper absolute paths
- Support multiple architectures (default: aarch64)

**Multi-architecture support:**
- Default: `aarch64` (ARM64 for modern Android devices)
- Other supported: `arm`, `x86_64`, `i686` (specify as second parameter)

**Dependencies:**
- Requires `dpkg-dev` for package building
- Binary must be pre-compiled for the target architecture

## Automated Building

All packaging formats are automatically built and uploaded as artifacts when creating a new release (git tag starting with 'v').

See `.github/workflows/release.yml` for the CI/CD configuration.

### CI/CD Environment Variables

The CI pipeline uses the following environment variables for maintainer information:
- `OSVM_MAINTAINER_NAME`: Defaults to "OpenSVM" 
- `OSVM_MAINTAINER_EMAIL`: Defaults to "rin@opensvm.com"

These can be overridden by setting repository secrets with the same names.

## Security and Best Practices

### Maintainer Information
- Maintainer details are centralized in `config.sh` for security and maintainability
- Use environment variables or repository secrets to override defaults in CI/CD
- Avoid hardcoding email addresses in scripts to prevent spam

### Package Security
- All packages include proper file permissions and ownership
- Termux packages use absolute paths to prevent path injection
- Debian packages follow FHS (Filesystem Hierarchy Standard)
- ArchLinux packages include proper checksums and validation

### Build Artifacts
- .deb files are placed in dedicated directories for reliable CI artifact collection
- Source tarballs exclude development files via `.gitattributes`
- SHA256 checksums are validated before use in package metadata

## Notes

- Debian packaging uses dh_make and dpkg-buildpackage with proper error handling
- ArchLinux packaging follows AUR standards with SHA256 validation
- Termux packaging creates Android-compatible packages with absolute paths
- All packages use centralized maintainer configuration for consistency
- Dependencies are automatically determined where possible
- Network-dependent tests are skipped during package builds for reliability

## Testing Packages

Before submitting packages to official repositories, test them locally:

1. Build the package using the appropriate script
2. Install it in a clean environment
3. Verify the binary works correctly
4. Test uninstallation procedures