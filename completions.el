;;; completions.el --- LSP and ctags configuration
;;; Commentary:
;;; Generic completion engines
;;; Code:
;; ctags setup (citre)
(use-package citre
  :ensure t

  :defer t
  :after projectile evil
  :bind (("C-c t j" . citre-jump)
         ("C-c t J" . citre-jump-back)
         ("C-c t r" . citre-peek-reference)
         ("C-c t p" . citre-ace-peek)
         ("C-c t u" . citre-update-this-tags-file)
         ("C-c t U" . citre-global-update-database))
  :init
  (require 'citre-config)
  ; Ctags config
  (setq citre-default-create-tags-file-location 'global-cache
        citre-project-root-function #'projectile-project-root
        citre-prompt-language-for-ctags-command t
        citre-use-project-root-when-creating-tags t
        ; Make completions easier to find
        citre-tags-completion-case-sensitive nil
        citre-capf-substr-completion t
        ; Disable capf integration since I handle it manually
        citre-enable-capf-integration nil
        ; Global configuration
        citre-gtags-args '("--compact"))
  (defun citre-use-global-windows ()
    (interactive)
    (setq-local citre-gtags-program "~/Global_669/bin/gtags.exe"
                citre-global-program "~/Global_669/bin/global.exe"))
  (evil-define-key 'normal 'citre-mode-map
    (kbd "g j") 'citre-jump
    (kbd "g J") 'citre-jump-back
    (kbd "g p") 'citre-peek
    (kbd "g P") 'citre-ace-peek))

;; LSP setup with lsp-mode
(use-package lsp-mode
  :ensure t
  :demand t
  :after consult corfu cape
  :bind (:map lsp-mode-map
         ("C-c C-." . lsp-execute-code-action))
  :init
  (setq lsp-completion-provider :none
        lsp-keymap-prefix "C-c l"
        lsp-pylsp-plugins-ruff-line-length 300)
  :config
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
  ; Use consult for lsp completions
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols)
  (defun corfu-lsp-setup ()
    "Enable lsp and its dependencies."
    (setf (alist-get 'styles
                     (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  (defun my/dont-launch-lsp-on-windows ()
    "Don't launch lsp on windows by default"
    (when (not (or (eq system-type 'ms-dos) (eq system-type 'windows-nt)))
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
  :demand t
  :config
  (global-tree-sitter-mode))
(use-package tree-sitter-langs
  :ensure t
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :after tree-sitter)
;;; completions.el ends here
