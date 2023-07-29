;;; init.el --- EMACS config v01302023
;;; Commentary:
;;; My Emacs configuration!
;;; Code:

;; Package signatures are broken in Windows
(if (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
    (setq package-check-signature nil
          gnutls-algorithm-priority "NORMAL")
    (setq network-security-level 'high
          gnutls-verify-error t
          gnutls-min-prime-bits 3072
          gnutls-algorithm-priority "PFS:-VERS-TLS1.2:-VERS-TLS1.1:-VERS-TLS1.0"))

;; Package management ;;
(require 'package)
(setq package-selected-packages
      '( use-package use-package-ensure-system-package
         gnu-elpa-keyring-update
         monokai-theme))
(setq package-native-compile t
      native-comp-async-report-warnings-errors nil
      package-quickstart t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(eval-when-compile
  (package-install-selected-packages t)
  (require 'use-package))

;;; Non-use-package configuration
;; Line numbering and word wrap
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)
(column-number-mode)
(transient-mark-mode 1)
(global-visual-line-mode)
;; Mac font sizing is unusually small
(if (eq system-type 'darwin)
    (setq font-height '120)
    (setq font-height '100))
(set-face-attribute 'default nil :height font-height :family "Hack")
(set-face-attribute 'fixed-pitch nil :family "Hack")
(set-face-attribute 'variable-pitch nil :family "Hack")
;; Set display of menu according with whether GUI is used
(when (and (or (eq system-type 'ms-dos) (eq system-type 'windows-nt)) (display-graphic-p))
  (menu-bar-mode 1))
;; Hide welcome screen
(setq inhibit-startup-screen t
      initial-scratch-message nil
      server-client-instructions nil)
;; Buffer settings
(setq use-short-answers t
      confirm-kill-processes nil
      kill-buffer-query-functions nil
      auth-source-save-behavior nil
      enable-local-variables :safe
      disabled-command-function nil)
;; Kill buffer command
(global-set-key (kbd "C-x k") 'kill-current-buffer)
;; Indent region command
(global-set-key (kbd "C-c i") 'indent-region)
;; Increase undo history
(setq undo-limit (* 4 1024 1024)
      undo-strong-limit (* 6 1024 1024)
      kill-ring-max 512
      kill-do-not-save-duplicates t)
;; Update files modified on disk
(setq global-auto-revert-non-file-buffers t)
;; Default to utf-8
(set-default-coding-systems 'utf-8)
;; Formatting
(setq-default fill-column 80
              indent-tabs-mode nil
              tab-width 4
              tab-always-indent nil
              require-final-newline t)
;(setq indent-line-function 'insert-tab)
(setq sentence-end-double-space nil)
;; Scrolling
(setq scroll-conservatively 101
      scroll-margin 0
      next-screen-context-lines 3)
;; Sandbox/suppress auto file creation
(setq auto-save-file-name-transforms
      `((".*" ,(file-name-concat user-emacs-directory "auto-save/") t))
      make-backup-files nil
      create-lockfiles nil
      custom-file null-device)
;; HTML
(add-to-list 'auto-mode-alist '("\\.html$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode))
;; Java
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . java-mode))
;; pascal, innosetup
(add-to-list 'auto-mode-alist '("\\.iss\\'" . pascal-mode))


(use-package use-package-ensure-system-package
  :ensure t)
(use-package diminish
  :ensure t)

;; Save minibuffer history
(use-package savehist
  :ensure t
  :config
  (savehist-mode))

(use-package ag
  :ensure t
  :ensure-system-package ag
  :config
  (use-package wgrep-ag
    :ensure t))

;; Theming ;;
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-molokai t))
(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-icon nil
        doom-modeline-height 17)
  :config
  (doom-modeline-mode 1))

(use-package adaptive-wrap
  :ensure t
  :config
  (setq adaptive-wrap-prefix-mode t))


;; Smart parens (select and use keybinds to place parens)
(use-package smartparens
  :ensure t
  :demand t
  :bind (("C-c (" . sp-wrap-round)
         ("C-c {" . sp-wrap-curly)))
(use-package evil-smartparens
  :ensure t
  :after smartparens
  :hook (smartparens-enabled . evil-smartparens-mode)
  :config
  (smartparens-global-mode 1)
  (smartparens-strict-mode 1))

;; Git config
(use-package magit-todos
  :ensure t)
(use-package magit
  :ensure t
  :after magit-todos
  :init
  (setq magit-view-git-manual-method 'man
        transient-history-file null-device
        magit-save-repository-buffers 'dontask
        magit-delete-by-moving-to-trash nil)
  :config
  ;; TODO convert this to use-package
  (with-eval-after-load 'magit
    (remove-hook 'server-switch-hook #'magit-commit-diff)
    (magit-todos-mode)))
(use-package git-gutter
  :ensure t
  :config
  (custom-set-variables '(git-gutter:update-interval 2))
  (custom-set-variables '(git-gutter:modified-sign " ")
                        '(git-gutter:added-sign " ")
                        '(git-gutter:deleted-sign " "))
  (set-face-background 'git-gutter:modified "purple")
  (set-face-background 'git-gutter:added "green")
  (set-face-background 'git-gutter:deleted "red")
  (global-git-gutter-mode t))


;; Basic error checking with flycheck
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

;; New formatting engine copied from doom, see https://editorconfig.org
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package avy
  :ensure t)
;; Evil
(use-package evil-collection
  :ensure t
  :demand t
  :diminish evil-collection-unimpaired-mode
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil)
  :config
  (evil-collection-init))
(use-package evil
  :ensure t
  :demand t
  :after evil-collection avy
  :config
  (evil-mode 1)
  ;; Allows redo functionality in evil
  ;; Only works in emacs 28 and later
  (evil-set-undo-system 'undo-redo)
  (evil-define-key 'normal 'global
    (kbd "SPC b") #'switch-to-buffer
    (kbd "SPC SPC") #'evil-avy-goto-word-or-subword-1
    (kbd "SPC s") #'evil-avy-goto-char-timer))
(use-package evil-mc
  :ensure t
  :demand t
  :after evil
  :config
  (evil-define-local-var evil-mc-custom-paused nil
    "Paused functionality when there are multiple cursors active.")
  (defun evil-mc-pause-smartchr-for-mode (mode)
    "Temporarily disable the smartchr keys for MODE."
    (let ((m-mode (if (atom mode) mode (car mode)))
          (s-mode (if (atom mode) mode (cdr mode))))
      (let ((init (intern (concat "smartchr/init-" (symbol-name s-mode))))
            (undo (intern (concat "smartchr/undo-" (symbol-name s-mode)))))
        (when (eq major-mode m-mode)
          (funcall undo)
          (push `(lambda () (,init)) evil-mc-custom-paused)))))
  (defun evil-mc-before-cursors-setup-hook ()
    "Hook to run before any cursor is created.
Can be used to temporarily disable any functionality that doesn't
play well with `evil-mc'."
    (mapc 'evil-mc-pause-smartchr-for-mode
          '(web-mode js2-mode java-mode (enh-ruby-mode . ruby-mode) css-mode))
    (when (boundp 'whitespace-cleanup-disabled)
      (setq whitespace-cleanup-disabled t)
      (push (lambda () (setq whitespace-cleanup-disabled nil)) evil-mc-custom-paused)))
  (defun evil-mc-after-cursors-teardown-hook ()
    "Hook to run after all cursors are deleted."
    (dolist (fn evil-mc-custom-paused) (funcall fn))
    (setq evil-mc-custom-paused nil))
  (add-hook 'evil-mc-before-cursors-created 'evil-mc-before-cursors-setup-hook)
  (add-hook 'evil-mc-after-cursors-deleted 'evil-mc-after-cursors-teardown-hook)
  (global-evil-mc-mode 1)
  (evil-define-key 'visual evil-mc-key-map
    "A" #'evil-mc-make-cursor-in-visual-selection-end
    "I" #'evil-mc-make-cursor-in-visual-selection-beg))
;(defvar evil-mc-mode-line-prefix "mc"
  ;"Override of the default mode line string for `evil-mc-mode'.")

;; Which key
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

;; Projectile
;; Set up projectile project directories in local.el!
(use-package projectile
  :ensure t
  :after evil
  :demand t
  :diminish projectile-mode
  :bind (:map projectile-mode-map
         ("C-c p" . projectile-command-map))
  :init
  (setq projectile-project-search-path '("~/.emacs.d"
                                        ("~/Documents/GitHub" . 1)))
  :config
  (projectile-mode +1)
  ; Bind the most useful projectile commands to easier keys
  (evil-define-key 'normal 'global
    (kbd "SPC f") #'projectile-find-file
    (kbd "SPC a") #'projectile-ag
    (kbd "SPC p") #'projectile-switch-project
    (kbd "SPC e") #'projectile-run-eshell))

;; ctags setup (citre)
(use-package citre
  :ensure t
  :demand t
  :after projectile evil
  :bind (("C-c t j" . citre-jump)
         ("C-c t J" . citre-jump-back)
         ("C-c t p" . citre-ace-peek)
         ("C-c t u" . citre-update-this-tags-file))
  :config
  (require 'citre-config)
  (setq citre-default-create-tags-file-location 'global-cache
        citre-project-root-function #'projectile-project-root
        citre-prompt-language-for-ctags-command t
        citre-use-project-root-when-creating-tags t)
  (evil-define-key 'normal 'citre-mode-map
    (kbd "g d") 'citre-jump
    (kbd "g D") 'citre-jump-back
    (kbd "g p") 'citre-peek
    (kbd "g P") 'citre-ace-peek
    (kbd "SPC u") 'citre-update-this-tags-file))

;; Virtico, Corfu, cape, orderless, consult, embark, marginalia
(use-package consult
  :ensure t
  :after evil
  :demand t
  :config
  (setq completion-in-region-function #'consult-completion-in-region)
  (evil-define-key 'normal 'global
    (kbd "SPC j") #'evil-collection-consult-jump-list))
(use-package consult-flycheck
  :ensure t
  :after consult flycheck)
(use-package cape
  :ensure t
  :after citre
  :init
  (setq cape-dabbrev-min-length 2)
  :config
  ;; For some reason cape doesn't have ispell on macos?
  (if (eq system-type 'darwin)
      (setq completion-at-point-functions (list #'cape-file
                                                (cape-super-capf #'cape-keyword
                                                                 #'cape-dabbrev
                                                                 #'citre-completion-at-point
                                                                 #'cape-dict)))
      (setq completion-at-point-functions (list #'cape-file
                                                (cape-super-capf #'cape-keyword
                                                                 #'cape-dabbrev
                                                                 #'citre-completion-at-point
                                                                 #'cape-ispell)))))
(use-package orderless
  :ensure t
  :init
  (setq orderless-component-separator #'orderless-escapable-split-on-space
        ;orderless-style-dispatchers '(+orderless-dispatch)
        completion-category-defaults nil
        ; I had this nil before
        completion-category-overrides '((file (styles . (partial-completion))))
        completion-styles '(orderless partial-completion basic)))
(use-package vertico
  :ensure t
  :config
  (vertico-mode))
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))
(use-package embark
  :ensure t)
(use-package embark-consult
  :ensure t
  :after embark consult)
(use-package corfu
  :ensure t
  :demand t
  :bind (:map corfu-map
              ("RET" . nil)
              ("M-p" . corfu-popupinfo-scroll-down)
              ("M-n" . corfu-popupinfo-scroll-up))
  :after consult cape orderless marginalia vertico embark
  :hook (corfu-mode . corfu-popupinfo-mode)
  :init
  (setq corfu-auto t
        corfu-auto-prefix 1
        read-extended-command-predicate #'command-completion-default-include-p)
  :config
  ;; Terminal specific settings for corfu
  (unless (display-graphic-p)
    (use-package corfu-terminal
      :ensure t
      :config
      (corfu-terminal-mode +1)))
  (global-corfu-mode))

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

;; Snippets
(use-package yasnippet-snippets
  :ensure t)
(use-package yasnippet
  :ensure t
  :after yasnippet-snippets
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))
(use-package lsp-mode
  :ensure t
  :autoload lsp
  :after consult corfu python-mode js-mode
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
  (add-hook 'js-mode-hook #'my/dont-launch-lsp-on-windows))
(use-package lsp-ui
  :ensure t
  :after lsp-mode)
(use-package consult-lsp
  :ensure t
  :after consult lsp-mode)

(use-package csv-mode
  :ensure t
  :hook (csv-mode . csv-align-mode))

(use-package markdown-mode
  :ensure t)

(use-package clang-format
  :ensure t)
(use-package cmake-mode
  :ensure t)

;; rust
(use-package rust-mode
  :ensure t
  :init
  (setq rust-format-on-save nil))
(use-package cargo
  :ensure t
  :after rust-mode
  :diminish cargo-mode-minor
  :hook ((rust-mode . cargo-mode-minor)
         (toml-mode . cargo-mode-minor))
  :config
  (dolist (sym '(rust-enable-format-on-save rust-disable-format-on-save))
    (put sym 'completion-predicate #'ignore)))

;; Org mode config x denote config
(use-package org-contrib
  :ensure t)
(use-package org
  :ensure t
  :after evil org-contrib
  :demand t
  :diminish org-indent-mode eldoc-mode auto-revert-mode
  ; Enable word wrap and org indenting
  :hook ((org-mode . toggle-truncate-lines)
         (org-mode . org-indent-mode))
  ;; Org keybinds
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map org-mode-map
         ("C-c ," . org-time-stamp-inactive)
         ("C-c ." . org-time-stamp)
         ("C-c x" . org-cut-subtree)
         ("S-<return>" . evil-org-open-below))
  :init
  ;; Set org directory
  (if (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
      (setq org-directory '"C:/org/org-notes")
      (setq org-directory '"~/Documents/GitHub/org-notes"))
  (setq org-enforce-todo-dependencies t
        org-enfocre-todo-checkbox-dependencies t
        diary-file (concat org-directory "/diary")
        appt-display-diary nil
        ;; Archive when closed
        org-todo-state-tags-triggers '(("CLOSED" ("ARCHIVE" . t)))
        org-hide-emphasis-markers t
        ;; Follow links on RET (cannot always use gx)
        org-return-follows-link t
        ;; Make org look better
        org-hide-leading-stars t
        ;; When a todo is set to done, add the completion time
        org-log-done 'note
        ;; Include diary stuff in the org agenda
        org-agenda-include-diary t
        ;; Set default directories, files
        org-default-notes-file (concat org-directory "/inbox.org")
        org-work-file (concat org-directory "/org-private/work.org")
        org-home-file (concat org-directory "/home.org")
        org-app-file (concat org-directory "/org-private/dnd-app.org")
        org-agenda-files (seq-filter
                          (lambda(x)
                            (not (string-match "/archive/"(file-name-directory x))))
                          (directory-files-recursively org-directory "\\.org$"))
        org-archive-location (concat "%s_archive::" org-directory "/archive")
        org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CLOSED"))
        org-capture-templates '(("t" "Inbox" entry (file+headline org-default-notes-file "Inbox")
                                 "* TODO %?\nDEADLINE: \n:PROPERTIES:\n:CREATION: %U\n:END:\n")
                                ("i" "Work Issue" entry (file+headline org-work-file "Inbox")
                                 "* TODO %?\nDEADLINE: \n:PROPERTIES:\n:CREATION: %U\n:TRACKZILLA: N/A\n:END:\n")
                                ("h" "Home Task" entry (file+headline org-home-file "Inbox")
                                 "* TODO %?\nDEADLINE: \n:PROPERTIES:\n:CREATION: %U\n:END:\n")
                                ("a" "App Task" entry (file+headline org-app-file "Inbox")
                                 "* TODO %?\nDEADLINE: \n:PROPERTIES:\n:CREATION: %U\n:END:\n")
                                ("j" "Journal" entry (file+datetree org-default-notes-file)
                                 "* %?\n%U\n%a")
                                ("n" "Link Inbox" entry (file+headline org-default-notes-file "Inbox")
                                 "* TODO %?\nDEADLINE: \n:PROPERTIES:\n:CREATION: %U\n:END:\n%a")
                                ("o" "Link Work Issue" entry (file+headline org-work-file "Inbox")
                                 "* TODO %?\nDEADLINE: \n:PROPERTIES:\n:CREATION: %U\n:END:\n%a")
                                ("m" "Link Home Task" entry (file+headline org-home-file "Tasks")
                                 "* TODO %?\nDEADLINE: \n:PROPERTIES:\n:CREATION: %U\n:END:\n%a")))
  :config
  (evil-define-key 'normal 'org-mode-map
    (kbd "SPC i") 'org-clock-in)
  (evil-define-key 'normal 'org-mode-map
    (kbd "SPC o") 'org-clock-out)
  (appt-activate 1)
  (require 'org-checklist)
  ;; Use org mode in org files
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode)))
; Denote settings
(use-package denote
  :ensure t
  :after evil org
  :demand t
  ; Denote formatting for files in dired
  :hook (dired-mode . denote-dired-mode)
  :init
  (setq denote-directory (expand-file-name "denote" org-directory)
        denote-prompts '(title keywords)
        denote-file-type 'org
        denote-known-keywords '("emacs" "work" "reflection" "denote" "politics" "philosophy" "recipe")
        denote-date-prompt-use-org-read-date t)
  :config
  (evil-define-key 'normal 'global
    (kbd "SPC n n") 'denote
    (kbd "SPC n r") 'denote-rename-file
    (kbd "SPC n l") 'denote-link))
;; Diary and calendar
(use-package calendar
  :ensure t
  :after org
  :bind (("C-x c" . calendar)
         ("C-x y" . diary))
  :hook ((today-visible-calendar . calendar-mark-today)
         (diary-display . fancy-diary-display)
         (list-diary-entries . sort-diary-entries)
         (list-diary-entries . include-other-diary-files)
         (mark-diary-entries . mark-include-diary-files))
  :init
  (setq view-diary-entries-initially t
        appt-display-mode-line t
        view-calendar-holidays-initially t
        mark-diary-entries-in-calendar t
        mark-holidays-in-calendar t))

;; TODO keywords in org
(use-package hl-todo
  :ensure t
  :init
  (setq hl-todo-keyword-faces
	    '(("TODO"           . "#FF0000")
	      ("IN-PROGRESS"    . "#A020F0")
	      ("WAITING"        . "#FF4500")
	      ("DONE"           . "#1E90FF")
          ("CLOSED"         . "#1E90FF")))
  :config
  (global-hl-todo-mode))
; Load org extensions
(use-package org-journal
  :ensure t
  :after org
  :bind ("C-c j j" . org-journal-new-entry)
  :init
  (setq org-journal-dir (concat org-directory "/journal")
        org-journal-prefix-key "C-c j "
        org-journal-enable-agenda-integration t
        org-journal-file-format "%Y-%m.org"
        org-journal-file-type 'monthly))
(use-package evil-org
  :ensure t
  :after org org-contrib
  :diminish evil-org-mode
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
  (evil-org-agenda-set-keys))

;; Obsidian
(use-package obsidian
  :ensure t
  :bind (("C-c o" . obsidian-search)
         :map obsidian-mode-map
         ("C-c C-l" . obsidian-insert-wikilink)
         ("C-<return>" . obsidian-follow-link-at-point))
  :init
  (setq obsidian-inbox-directory "Inbox"
        obsidian-path (if (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
                          "C:/Users/dillona/Documents/projects/0notes/Notes/Notes"
                        "~/Documents/Obsidian/notes"))
  :config
  (obsidian-specify-path obsidian-path)
  (global-obsidian-mode t))

;; SQL
(setq sql-mysql-options '("--prompt=mysql> " "-C" "-t" "-f" "-n"))
(add-hook 'sql-mode-hook (lambda ()
                           (require 'sql)
                           (sql-highlight-mariadb-keywords)))
(add-hook 'sql-interactive-mode-hook (lambda () (toggle-truncate-lines t)))

;; eshell
(use-package eshell
  :bind ("C-x e" . eshell)
  :hook (eshell-mode . (lambda ()
    ; Quick switch to org
    (eshell/alias "so"
        (if (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
            "cd C:/org/org-notes"
            "cd ~/Documents/GitHub/org-notes"))
    ; Opening files
    (eshell/alias "e" "find-file $1")
    (eshell/alias "ff" "find-file $1")
    (eshell/alias "emacs" "find-file $1")
    (eshell/alias "ee" "find-file-other-window $1")
    ; Just in case an oopsie occurs
    (eshell/alias "vim" "find-file-other-window $1")
    (eshell/alias "vi" "find-file-other-window $1")
    (eshell/alias "less" "find-file-other-window $1")
    ; Open in explorer
    (eshell/alias "ex" "explorer .")
    ; Git
    (eshell/alias "gd" "magit-diff-unstaged")
    (eshell/alias "gds" "magit-diff-staged")
    ; Dired
    (eshell/alias "d" "dired .")
    ; The 'ls' executable requires the Gnu version on the Mac
    (let ((ls (if (file-exists-p "/usr/local/bin/gls")
                  "/usr/local/bin/gls"
                "ls")))
      (eshell/alias "ll" (concat ls " -AlohG --color=always"))))))

;; Garbage collection
(use-package gcmh
  :ensure t
  :diminish gcmh-mode
  :init
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold (* 32 1024 1024))
  :config
  (gcmh-mode 1))

;; Load custom settings
(let ((local-settings "~/.emacs.d/local.el"))
  (when (file-exists-p local-settings)
    (load-file local-settings)))

;; Open init.el on opening
(set-register ?e (find-file (or user-init-file "")))

;; Editor package management section
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; init.el ends here
