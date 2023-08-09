;;; completions.el --- LSP and ctags configuration
;;; Commentary:
;;; citre, lsp-mode, lsp-ui, consult-lsp, tree-sitter, tree-sitter-langs
;;; Code:
;; ctags setup (citre)
(use-package citre
  :ensure t
  :pin melpa
  :defer t
  :after projectile evil
  :bind (("C-c t j" . citre-jump)
         ("C-c t J" . citre-jump-back)
         ("C-c t r" . citre-peek-reference)
         ("C-c t p" . citre-ace-peek)
         ("C-c t u" . citre-update-this-tags-file))
  :init
  (require 'citre-config)
  (setq citre-default-create-tags-file-location 'global-cache
        citre-project-root-function #'projectile-project-root
        citre-prompt-language-for-ctags-command t
        citre-use-project-root-when-creating-tags t
        citre-gtags-args '("--compact"))
  (evil-define-key 'normal 'citre-mode-map
    (kbd "g d") 'citre-jump
    (kbd "g D") 'citre-jump-back
    (kbd "g p") 'citre-peek
    (kbd "g P") 'citre-ace-peek
    (kbd "SPC u") 'citre-update-this-tags-file))

;; LSP setup with lsp-mode
(use-package lsp-mode
  :ensure t
  :demand t
  :after consult corfu
  :init
  (setq lsp-completion-provider :none
        lsp-keymap-prefix "C-c l")
  :config
  ; Use consult for lsp completions
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols)
  (defun corfu-lsp-setup ()
    "Enable lsp and its dependencies."
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  (defun my/dont-launch-lsp-on-windows ()
    "Don't launch lsp on windows by default"
    (if (not (or (eq system-type 'ms-dos) (eq system-type 'windows-nt)))
        (lsp)))
  (add-hook 'lsp-completion-mode #'corfu-lsp-setup)
  (add-hook 'python-mode-hook #'my/dont-launch-lsp-on-windows)
  (add-hook 'html-mode-hook #'my/dont-launch-lsp-on-windows)
  (add-hook 'js-mode-hook #'my/dont-launch-lsp-on-windows))
(use-package lsp-ui
  :ensure t
  :after lsp-mode)
(use-package consult-lsp
  :ensure t
  :after consult lsp-mode)

;; Tree sitter
(use-package tree-sitter
  :ensure t
  :diminish tree-sitter-mode
  :config
  (global-tree-sitter-mode))
(use-package tree-sitter-langs
  :ensure t
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :after tree-sitter)
;;; completions.el ends here
