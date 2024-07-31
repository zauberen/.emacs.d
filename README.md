# emacs-config
My emacs configuration

## Install instructions
1. Install emacs on system
2. Install dependencies
   - `ispell` (non-macos) (causes completion errors without it)
   - Universal `ctags` (tested with 6.0) (provides completion and seek when lsp is not available)
   - `python3`(.10+ preferred) (python lsp) then run `pip3 install "python-lsp-server[all]"` (python lsp)
   - `dasel` for python venv support
   - `node` (tested with 20.3) (lsp)
   - `ag` or `the_silver_searcher`(macos) or `silversearcher-ag`(ubuntu) (project searching) (optional)
     - Plugin makes a buffer which is easy to search through
   - `rg` or `ripgrep` (project searching) (optional)
     - Built into consult, replaces git-grep when installed.
     - Similar functionality to ag by using embark-collect (C-s to search, then M-S-; to collect)
   - `fonts-hack-ttf`(ubuntu) or `homebrew/cask-fonts/font-hack`(macos) Hack font
   - Some LaTeX distribution (recommend `texlive`)
   - `pandoc` (optional but recommended) for exporting files
   - `emacs-lsp-booster` (optional, non-windows) Optimizes lsp-mode, but requires a full plugin reinstall to work. The booster exe is included in this repo for windows.
     - Install rust: `curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh`
     - Get the code: `git clone https://github.com/blahgeek/emacs-lsp-booster`
     - Build: `cd emacs-lsp-booster; cargo build --release`
     - The binary is here: `target/release/emacs-lsp-booster`
     - Move the `emacs-lsp-booster` binary to a directory on the path (eg `~/.local/bin`)
3. Create .emacs.d folder (varies) and dump this repo into it
4. Create `.emacs.d/auto-save`
5. Start emacs (first load will take a while while it loads and compiles plugins)
6. Run `all-the-icons-install-fonts` to install the emoji and icon fonts for the emacs UI
