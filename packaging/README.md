# OSVM CLI Packaging

This directory contains packaging scripts and configuration for building OSVM CLI packages for different platforms.

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
- Set up proper Termux directory structure
- Include install/remove scripts

## Automated Building

All packaging formats are automatically built and uploaded as artifacts when creating a new release (git tag starting with 'v').

See `.github/workflows/release.yml` for the CI/CD configuration.

## Notes

- Debian packaging uses dh_make and dpkg-buildpackage
- ArchLinux packaging follows AUR standards
- Termux packaging creates Android-compatible packages
- All packages set maintainer to "OpenSVM <rin@opensvm.com>"
- Dependencies are automatically determined where possible

## Testing Packages

Before submitting packages to official repositories, test them locally:

1. Build the package using the appropriate script
2. Install it in a clean environment
3. Verify the binary works correctly
4. Test uninstallation procedures