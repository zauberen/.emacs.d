# emacs-config
My emacs configuration

This branch is a WIP conversion to elpaca.el from package.el

## Install instructions
1. Install emacs on system
2. Install dependencies
   - `ispell` (non-macos) (causes completion errors without it)
   - Universal `ctags` (tested with 6.0) (provides completion and seek when lsp is not available)
   - `python3`(.10+ preferred) (python lsp) pip3 install "python-lsp-server[all]" (python lsp)
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
3. Create .emacs.d folder (varies) and dump this repo into it
4. Move `root.editorconfig` to `~/.editorconfig`
5. Create `.emacs.d/auto-save`
6. Start emacs (first load will take a while while it loads and compiles plugins)
