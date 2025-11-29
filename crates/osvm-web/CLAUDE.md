# OSVM-Web Crate - CLAUDE.md

## Overview

**OSVM-Web** is a WebAssembly-based browser implementation of the OSVM Research TUI, enabling blockchain investigation directly in the browser via WebGL2 rendering.

## Architecture

```
src/
├── lib.rs    # WASM entry point, app state, event handling (~48K lines)
└── graph.rs  # Transaction graph visualization (~107K lines)
```

## Key Components

### Main App (`lib.rs`)
- WASM bindings via `wasm-bindgen`
- Ratzilla-based TUI rendering (ratatui for web)
- Keyboard/touch event handling
- API calls to backend via `gloo-net`
- URL hash state for sharing investigations

### Graph Visualization (`graph.rs`)
- Transaction flow graph rendering
- Node layout algorithms (force-directed, columnar)
- Edge rendering with amount labels
- Zoom/pan controls
- AI insights panel integration

## Building

```bash
# Install wasm-pack if not present
cargo install wasm-pack

# Build for web
cd crates/osvm-web
wasm-pack build --target web --release

# Output in pkg/
# - osvm_web.js
# - osvm_web_bg.wasm
# - osvm_web.d.ts
```

## Usage

```html
<!DOCTYPE html>
<html>
<head>
  <style>
    body { margin: 0; background: #1a1a2e; }
    #terminal { width: 100vw; height: 100vh; }
  </style>
</head>
<body>
  <div id="terminal"></div>
  <script type="module">
    import init, { OsvmApp } from './pkg/osvm_web.js';

    async function run() {
      await init();
      const app = new OsvmApp('terminal');
      app.start();
    }
    run();
  </script>
</body>
</html>
```

## Features

### Implemented
- Full TUI in browser (WebGL2 rendering)
- Wallet investigation mode
- Transaction graph visualization
- Token holdings display
- AI insights panel
- Touch support for mobile
- URL hash state persistence

### Touch Gestures
- Single tap: Select
- Double tap: Zoom in
- Two-finger pinch: Zoom in/out
- Two-finger pan: Move view
- Swipe: Navigate tabs

## Key Differences from CLI

| Feature | CLI | Web |
|---------|-----|-----|
| Rendering | Terminal escape codes | WebGL2 via Ratzilla |
| HTTP | reqwest (native) | gloo-net (fetch API) |
| Storage | Filesystem | LocalStorage |
| Input | stdin | DOM events |

## Dependencies

- `ratzilla` - ratatui for WASM/WebGL2
- `wasm-bindgen` - Rust/JS interop
- `gloo-net` - HTTP fetch wrapper
- `gloo-timers` - setTimeout/setInterval
- `web-sys` - DOM API bindings
- `js-sys` - JavaScript runtime bindings

## API Endpoints

The web app expects a backend at `/api/`:

```
GET  /api/wallet/{address}          - Wallet info
GET  /api/wallet/{address}/transfers - Transfer history
GET  /api/wallet/{address}/tokens   - Token holdings
POST /api/research                  - AI research query
```

## Size Optimization

The release build uses aggressive optimization:
- `opt-level = "z"` - Optimize for size
- `lto = true` - Link-time optimization
- `codegen-units = 1` - Better optimization
- `panic = "abort"` - Remove unwinding
- `strip = true` - Remove symbols

Typical output: ~800KB WASM (gzipped: ~250KB)

## Development

```bash
# Dev build with debug info
wasm-pack build --target web --dev

# Watch mode (requires cargo-watch)
cargo watch -w src -s "wasm-pack build --target web --dev"

# Test in browser
python3 -m http.server 8080
# Open http://localhost:8080
```

## Known Limitations

1. **No native Solana RPC** - Requires backend proxy for RPC calls
2. **No filesystem** - Can't save files, use LocalStorage or download
3. **WebSocket limitations** - Some browsers limit concurrent connections
4. **Mobile keyboards** - Virtual keyboard may cover UI

## Debugging

```javascript
// In browser console
localStorage.setItem('OVSM_DEBUG', 'true');
location.reload();

// Check WASM memory
console.log(wasmModule.memory.buffer.byteLength);
```
