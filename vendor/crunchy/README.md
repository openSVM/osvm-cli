# Fix for crunchy crate cross-compilation issue

This directory contains a patched version of the `crunchy` crate that fixes a Windows cross-compilation issue.

## Problem

The original `crunchy` v0.2.3 crate has a bug when cross-compiling from Unix systems to Windows targets. 
The issue is in `src/lib.rs` lines 36-40:

```rust
#[cfg(target_os = "windows")]
include!(concat!(env!("OUT_DIR"), "\\lib.rs"));

#[cfg(not(target_os = "windows"))]
include!(concat!(env!("OUT_DIR"), "/lib.rs"));
```

When cross-compiling from Linux to Windows, `target_os = "windows"` is true, but the build script runs on Linux, 
so `OUT_DIR` contains a Linux path like `/home/.../target/x86_64-pc-windows-gnu/debug/build/crunchy-xxx/out`. 
Appending `\\lib.rs` creates an invalid path: `/home/.../out\lib.rs`.

## Solution

The fix uses forward slashes consistently since the build process always runs on the host system:

```rust
// Always use forward slash since this runs on the host system during build
include!(concat!(env!("OUT_DIR"), "/lib.rs"));
```

## Error Fixed

This resolves the error:
```
error: couldn't read `/github/workspace/./target/x86_64-pc-windows-gnu/release/build/crunchy-034bbf831537023a/out\lib.rs`: No such file or directory (os error 2)
```

## Source

Original crate: https://github.com/eira-fransham/crunchy
Version: 0.2.3
License: MIT