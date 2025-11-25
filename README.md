# EMACS configuration
My emacs configuration. Generally supports only the latest stable release of emacs.

## Install instructions
1. Install emacs on system (unless latest stable is in the package repos, you will need to install from source, don't use flatpak or snap if possible. See https://write.as/zauberin/building-emacs-from-source for build instructions) 
2. Install dependencies
   - Universal `ctags` (tested with 6.0) (provides completion and seek when lsp is not available)
   - Install libtree-sitter (libtree-sitter-dev with apt)
     - May also compile from source, see https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
   - `python3`(.10+ preferred) (python lsp) then run `pip3 install "python-lsp-server[all]"` (python lsp)
     - If you want debugging support with DAP, you will also need `ptvsd`
   - `dasel` for python venv support
   - `node` (tested with 20.3) (lsp)
   - `rg` or `ripgrep` (project searching) (optional)
     - Built into consult, replaces git-grep when installed.
     - Get your search in a buffer using deadgrep (C-M-s)
     - Similar functionality to deadgrep by using embark-collect (C-s to search, then M-S-; to collect)
   - `fonts-hack-ttf`(ubuntu) or `homebrew/cask-fonts/font-hack`(macos) Hack font
   - Some LaTeX distribution (recommend `texlive`)
   - `pandoc` (optional but recommended) for exporting files
   - `ispell` (non-macos) (causes completion errors without it)
   - For clojure support
     - `leiningen` (also released as `lein`, and is that on the path)
     - `clojure`
     - `hiccup-cli` (optional) also requires elpaca-try hiccup-cli. Adds better hiccup conversion support. Must install manually either by compiling from source or by downloading and adding the binary to path.
   - `vale` A spell checker for writing. See https://github.com/errata-ai/vale
     - To initialize vale in a new directory, create a `.vale.ini` in the git home directory, a generator is here: https://vale.sh/generator
     - Next run `vale sync` in the git home directory, and it will be ready!
   - `emacs-lsp-booster` (binaries included in lsp folder) Optimizes lsp-mode, but requires a full plugin reinstall to work.
     - If you want to build the booster binaries, follow these steps:
     - Install rust: `curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh`
     - Get the code: `git clone https://github.com/blahgeek/emacs-lsp-booster`
     - Build: `cd emacs-lsp-booster; cargo build --release`
     - The binary is here: `target/release/emacs-lsp-booster`
     - Move the `emacs-lsp-booster` binary into the `.emacs.d/lsp` folder, replace the existing one. Note that each platform has a different spot.
3. Create .emacs.d folder (varies) and dump this repo into it
5. Start emacs (first load will take a while while it loads and compiles plugins)
6. Run `all-the-icons-install-fonts` to install the emoji and icon fonts for the emacs UI
7. Download the latest `tree-sitter-langs` from https://github.com/emacs-tree-sitter/tree-sitter-langs/releases and put the appropriate `tar.gz` release into the `~/.emacs.d/tree-sitter` folder, then:
``` shell
tar xzf tree-sitter-grammars
rename 's/^/libtree-sitter-/' *.so
```
8. Run `fontaine-set-preset` and pick hack-ttf, there is also a hack-ttf-mac which is better on MacOS since fonts appear smaller there.
9. `MacOS`: You may have compilation issues if the first launch is not from the terminal. Generally, you will always want to launch from the terminal due to permission issues during launch otherwise.
10. Restart emacs
11. Run `treesit-auto-install-all` to install languages not covered by the packaged grammars.
