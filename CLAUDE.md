# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

A personal Emacs configuration targeting the latest stable Emacs release. Uses [elpaca](https://github.com/progfolio/elpaca) as the package manager with `use-package` integration.

## Testing changes

There is no automated test suite. To validate changes, start Emacs and check for errors:

```shell
emacs --debug-init
```

To reload a single file without restarting:
```
M-x load-file RET ~/.emacs.d/<file>.el RET
```

To evaluate a single expression: place cursor at end and use `C-x C-e`, or use `M-x eval-buffer`.

## File structure and architecture

`init.el` is the entry point. It loads modules in this order:

| File | Contents |
|------|----------|
| `elpaca.el` | Package manager bootstrap (do not modify the installer block) |
| `tweaks.el` | Pure Emacs settings, no packages |
| `evil.el` | evil, evil-collection, evil-mc, smartparens, tree-sitter text objects |
| `git.el` | magit, transient, diff-hl, vc-msg |
| `snippets.el` | yasnippet, tempel |
| `hydra.el` | hydra definitions |
| `completions.el` | citre (ctags/gtags), dumb-jump, lsp-mode, lsp-ui, consult-lsp |
| `minibuffer.el` | consult, vertico, marginalia, embark |
| `corfu.el` | cape, corfu (in-buffer completion with capf) |
| `lang.el` | Language-specific config: Java/DAP, Lisp/CIDER/SLY, Python, Rust, web, SQL, PHP, NASM, YAML, tree-sitter |
| `org.el` | org, evil-org, denote, denote-explore, consult-notes, org-super-agenda, calendar |
| `obsidian.el` | obsidian.el integration |
| `editor.el` | Bibliography and other text editor functionality |
| `eshell.el` | Eshell configuration |
| `ai.el` | gptel, copilot-chat, claude-code-ide |
| `exwm.el` | Only loaded if exwm is already installed |
| `fun.el` | Misc fun packages |

**`local.el`** — machine-specific settings loaded last (if it exists). API keys, database connections, and per-machine overrides go here. It is gitignored. `local.el` can use `:ensure nil` `use-package` blocks to extend packages already configured in the main files.

## Key architectural patterns

### Package installation
All packages use `(use-package NAME :ensure t ...)`. Packages from GitHub use `:ensure (:host github :repo "user/repo")`. The `elpaca-wait` call at the end of `init.el` ensures all packages are installed before `local.el` loads.

### Evil keybindings
Evil bindings are added with `evil-define-key` inside `:config` blocks. The main normal-mode leader key is `SPC`. Insert state uses mostly default Emacs bindings (intentionally; `evil-disable-insert-state-bindings t`).

### Completion stack
- **Minibuffer**: vertico + orderless + marginalia + consult + embark
- **In-buffer**: corfu (popup) + cape (capf combinators)
- `my/build-capf` in `corfu.el` assembles per-mode capf lists using `cape-capf-super`
- LSP completions go through `lsp-completion-at-point`, wrapped by cape

### LSP setup
`lsp-mode` is in `completions.el`. Language-specific LSP hooks are in `lang.el`. On Windows, LSP is not auto-started for most languages (`my/dont-launch-lsp-on-windows`). Java LSP only auto-starts if `*jdtls*` buffer already exists.

### AI integration (ai.el)
- **gptel**: general LLM chat, configured for org-mode output. AI backends (DeepSeek, Gemini, Ollama) are set up via helper functions `setup-deep-seek`, `setup-gemini`, `setup-ollama` — call these with API keys in `local.el`.
- **claude-code-ide**: this Claude Code integration; uses `eat` as terminal backend.

### Platform conditionals
Code checks `system-type` for `'ms-dos`/`'windows-nt` (Windows), `'darwin` (macOS), and `'gnu/linux`. Windows has many workarounds (elpaca queue limit, no symlinks, slower LSP delays, etc.).

### org-directory
Set in `init.el` before module loading. Linux/macOS: `~/Documents/GitHub/org-notes`. Windows: `C:/org/org-notes`. Override this in `local.el` if needed.

## Important conventions

- All `.el` files use `;;; lexical-binding: t` and follow standard Emacs file conventions (`;;; filename.el --- description`, `(provide 'filename)`, `;;; filename.el ends here`).
- Do not add packages to `init.el` directly — add them to the appropriate module file.
- Prefer `use-package` with `:ensure t` for all external packages.
- Machine-specific config (paths, API keys, DB connections) belongs in `local.el`, not in the tracked files.
- The `elpa/installed.txt` sentinel file tracks whether elpaca has done initial install. Delete it to force a re-install.
