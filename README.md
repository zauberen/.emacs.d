# emacs-config
My emacs configuration

## Install instructions
1. Install emacs on system
2. Install dependencies
   - `ispell` (non-macos) (causes completion errors without it)
   - Universal `ctags` (tested with 6.0) (provides completion and seek when lsp is not available)
   - `python3`(.10+ preferred) (python lsp) pip3 install "python-lsp-server[all]" (python lsp)
   - `node` (tested with 20.3) (lsp)
   - `ag` or `the_silver_searcher`(macos) or `silversearcher-ag`(ubuntu) (project searching)
   - `fonts-hack-ttf`(ubuntu) or `homebrew/cask-fonts/font-hack`(macos) Hack font
3. Create .emacs.d folder (varies) and dump this repo into it
4. Move root.editorconfig to ~/.editorconfig
5. Create .emacs.d/auto-save
6. Start emacs (first load will take a while while it loads and compiles plugins)
