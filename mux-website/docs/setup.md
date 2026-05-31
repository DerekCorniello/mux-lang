# Setup

This page covers end-to-end Mux setup, including runtime installs, compiler/tooling, and editor integration.

## Language install

### Runtime (prebuilt binaries)

Use the prebuilt installer to get the runtime without Rust or LLVM.

**Linux and macOS:**
```bash
curl -fsSL https://raw.githubusercontent.com/DerekCorniello/mux-lang/main/scripts/install.sh | sh
```

**Windows (PowerShell):**
```powershell
iwr -useb https://raw.githubusercontent.com/DerekCorniello/mux-lang/main/scripts/install.ps1 | iex
```

**Custom install directories:**
```bash
MUX_INSTALL_DIR=/usr/local/bin MUX_LIB_DIR=/usr/local/lib sh install.sh
```

### Compiler and tooling (source builds)

For compiler development or source builds, you need LLVM 17 and clang. The bootstrap script installs the toolchain automatically.

```bash
git clone https://github.com/DerekCorniello/mux-lang
cd mux-lang
./scripts/bootstrap-dev.sh
./scripts/dev-cargo.sh build
```

### Verify installation

```bash
mux --version
mux doctor
mux doctor --dev
```

## Syntax highlighting

Mux ships TextMate and Tree-sitter grammars. Use the section that matches your editor.

### TextMate family (VSCode, Sublime Text, JetBrains)

**Generate grammar (required before packaging):**
```bash
cd mux-syntax-highlighting
node scripts/generate-syntax.js
```

**VSCode:**
1. Package the extension:
   ```bash
   ../scripts/release-syntax.sh
   ```
2. Install the `.vsix`:
   ```bash
   code --install-extension language-mux-<version>.vsix
   ```
3. Reload VSCode.

**Sublime Text:**
1. Copy `mux-syntax-highlighting/textmate-mux/source.mux.json` into `Packages/User/Mux/`.
2. Add to `Packages/User/Package.sublime-settings`:
   ```json
   {
     "syntax": [
       {
         "name": "Mux",
         "scope": "source.mux",
         "file_extensions": [".mux"],
         "path": "User/Mux/source.mux.json"
       }
     ]
   }
   ```

**JetBrains (IntelliJ, WebStorm, etc.):**
1. Install the TextMate Bundles plugin.
2. Import `mux-syntax-highlighting/textmate-mux/source.mux.json`.
3. Associate `.mux` files in Settings > Editor > File Types.

### Tree-sitter family (Neovim, Helix)

**Generate grammar (required before packaging):**
```bash
cd mux-syntax-highlighting
node scripts/generate-syntax.js
```

**Neovim:**
1. Install the parser and queries from `mux-syntax-highlighting/tree-sitter-mux/`.
2. Add to `~/.config/nvim/init.lua`:
   ```lua
   local parser_config = require "nvim-treesitter.parsers".get_parser_configs()
   parser_config.mux = {
     install_info = {
       url = "https://github.com/DerekCorniello/mux-lang",
       files = {"mux-syntax-highlighting/tree-sitter-mux/grammar.js"},
       branch = "main"
     },
     filetype = "mux",
   }
   ```
3. Run `:TSInstall mux` and enable highlighting.

**Helix:**
1. Build the parser:
   ```bash
   mkdir -p ~/.config/helix/runtime/grammars
   cd mux-syntax-highlighting/tree-sitter-mux
   tree-sitter generate grammar.js
   cp mux.so ~/.config/helix/runtime/grammars/
   ```
2. Add to `~/.config/helix/languages.toml`:
   ```toml
   [[language]]
   name = "mux"
   scope = "source.mux"
   file-types = ["mux"]
   roots = []
   grammar = true

   [language.highlight]
   paths = ["queries/highlights.scm"]
   ```

## LSP

In development. There is no supported Mux LSP release yet.

## Playground local development

To run the docs site and compiler API locally for playground testing:

1. Start the API server from the repo root:

```bash
uv run python api/server.py
```

2. In a second terminal, start the website:

```bash
cd mux-website
npm start
```

The site runs on `http://localhost:3000` and uses `http://localhost:8080` for the playground API by default.

If you want to point the website at a different API URL, set this before starting the docs site:

```bash
MUX_API_URL=https://your-api-url npm start
```

## Profiling

Profiling is done with external tools so it stays decoupled from the compiler and runtime.

**Linux:**
- `perf` + flamegraph

**macOS:**
- Instruments

**Windows:**
- Windows Performance Analyzer or Visual Studio Profiler
