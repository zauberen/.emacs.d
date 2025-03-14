;;; completions.el --- Completion and syntax checking packages -*- lexical-binding: t -*-
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
  ;; This code is used to optimize the lsp interaction with emacs
  ;; Code adapted from https://github.com/blahgeek/emacs-lsp-booster
    (defun lsp-booster--advice-json-parse (old-fn &rest args)
      "Try to parse bytecode instead of json."
      (or
       (when (equal (following-char) ?#)
         (let ((bytecode (read (current-buffer))))
           (when (byte-code-function-p bytecode)
             (funcall bytecode))))
       (apply old-fn args)))
    (advice-add (if (progn (require 'json)
                           (fboundp 'json-parse-buffer))
                    'json-parse-buffer
                  'json-read)
                :around
                #'lsp-booster--advice-json-parse)

    (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
      "Prepend emacs-lsp-booster command to lsp CMD."
      (let ((orig-result (funcall old-fn cmd test?)))
        (if (and (not test?)                             ;; for check lsp-server-present?
                 (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
                 lsp-use-plists
                 (not (functionp 'json-rpc-connection))  ;; native json-rpc
                 ;; For windows, the exe is in .emacs.d
                 (if (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
                     (file-exists-p (expand-file-name "lsp/emacs-lsp-booster.exe" user-emacs-directory))
                   (if (eq system-type 'darwin)
                       (file-exists-p (expand-file-name "lsp/macos/emacs-lsp-booster" user-emacs-directory))
                       (file-exists-p (expand-file-name "lsp/linux/emacs-lsp-booster" user-emacs-directory)))))
            (progn
              (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
                (setcar orig-result command-from-exec-path))
              (message "Using emacs-lsp-booster for %s!" orig-result)
              (cons (if (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
                        (expand-file-name "lsp/emacs-lsp-booster.exe" user-emacs-directory)
                      (if (eq system-type 'darwin)
                          (expand-file-name "lsp/macos/emacs-lsp-booster" user-emacs-directory)
                        (expand-file-name "lsp/linux/emacs-lsp-booster" user-emacs-directory)))
                    orig-result))
          orig-result)))
    (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
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
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))
(use-package tree-sitter-langs
  :ensure t
  :config
  (global-tree-sitter-mode))
;;; completions.el ends here
