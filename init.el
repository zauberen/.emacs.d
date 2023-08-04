;;; init.el --- EMACS config
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
         gnu-elpa-keyring-update))
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
; Fixes some auto save issues with encoding
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
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


(use-package use-package-ensure-system-package
  :ensure t)
(use-package diminish
  :ensure t)

;; Save minibuffer history
(use-package savehist
  :ensure t
  :config
  (savehist-mode))

;; ag, the silver searcher
(use-package ag
  :ensure t
  :ensure-system-package ag)
(use-package wgrep-ag
  :ensure t)

;; Theming ;;
; I really like ef-elea-dark
(use-package ef-themes
  :config
  ;(when (not (eq system-type 'darwin))
  (load-theme 'ef-elea-dark t)
  :ensure t)
; My original emacs theme
(use-package molokai-theme
  :config
  ;(load-theme 'molokai-theme t)
  :ensure t)
; Doom themes, I like doom-molokai
(use-package doom-themes
  :config
  ;(when (eq system-type 'darwin)
    ;(load-theme 'doom-molokai t))
  :ensure t)
(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-icon nil
        doom-modeline-height 17)
  :config
  (doom-modeline-mode 1))

(use-package adaptive-wrap
  :ensure t
  :demand t
  :config
  (setq adaptive-wrap-prefix-mode t))


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
  :demand t
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
  :demand t
  :ensure t
  :config
  (global-flycheck-mode))

;; New formatting engine copied from doom, see https://editorconfig.org
(use-package editorconfig
  :ensure t
  :demand t
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
  ;; Allows redo functionality in evil
  ;; Only works in emacs 28 and later
  (evil-set-undo-system 'undo-redo)
  (evil-define-key 'normal 'global
    (kbd "SPC b") #'switch-to-buffer
    (kbd "SPC SPC") #'evil-avy-goto-word-or-subword-1
    (kbd "SPC s") #'evil-avy-goto-char-timer)
  (evil-mode 1))
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

;; Smart parens (select and use keybinds to place parens)
(use-package smartparens
  :ensure t
  :demand t
  :bind (("C-c (" . sp-wrap-round)
         ("C-c {" . sp-wrap-curly)
         ("C-c [" . sp-wrap-square)))
(use-package evil-smartparens
  :ensure t
  :demand t
  :after smartparens evil
  :hook (smartparens-enabled . evil-smartparens-mode)
  :config
  (smartparens-global-mode 1)
  (smartparens-strict-mode 1))

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
  :demand t
  :after consult flycheck)
(use-package cape
  :ensure t
  :after citre
  :init
  (setq cape-dabbrev-min-length 2)
  :config
  (if (eq system-type 'darwin)
      (setq completion-at-point-functions (list #'cape-file
                                                (cape-super-capf #'cape-keyword
                                                                 #'cape-dabbrev
                                                                 #'citre-completion-at-point
                                                                 #'cape-dict)))
      (setq completion-at-point-functions (list #'cape-file
                                                (cape-super-capf #'cape-keyword
                                                                 #'cape-dabbrev
                                                                 #'citre-completion-at-point)))))
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
  :demand t
  :config
  (vertico-mode))
(use-package marginalia
  :ensure t
  :demand t
  :config
  (marginalia-mode))
(use-package embark
  :ensure t
  :demand t
  :after evil
  ; Bind both C-c e (better pneumonics) and C-c v (matches evil better)
  :bind (("C-c e" . embark-act)
         ("C-c v" . embark-act)
         ("C-;" . embark-act))
  :config
  (evil-define-key 'normal 'global
    (kbd "SPC v") #'embark-act))
(use-package embark-consult
  :ensure t
  :demand t
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
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (when completion-in-region--data
      (let ((completion-extra-properties corfu--extra)
            completion-cycle-threshold completion-cycling)
        (apply #'consult-completion-in-region completion-in-region--data))))
  (define-key corfu-map (kbd "C-c m") #'corfu-move-to-minibuffer)
  (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer)
  ;; Terminal specific settings for corfu
  (global-corfu-mode))
(use-package corfu-terminal
  :ensure t
  :demand t
  :after corfu
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

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
  :demand t
  :after yasnippet-snippets
  :diminish yas-minor-mode
  :bind ("C-c y" . yas-insert-snippet)
  :config
  (evil-define-key 'normal 'global
    (kbd "SPC y") #'yas-insert-snippet)
  (yas-global-mode 1))

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

(use-package html-mode
  :mode (("\\.html$" . html-mode)
         ("\\.html\\'" . html-mode)))

(use-package java-mode
  :mode ("\\.jsp\\'" . java-mode))

(use-package csv-mode
  :ensure t
  :hook (csv-mode . csv-align-mode)
  :mode "\\.csv\\'")

;; pascal, innosetup
(use-package pascal-mode
  :mode ("\\.iss\\'" . pascal-mode)
  ;:hook (pascal-mode . (remove-hook 'completion-at-point-functions 'pascal-completions-at-point t))
  :init
  (setq pascal-indent-level 4))

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'". gfm-mode)
         ("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown"))

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
  :ensure t
  :demand t)
(use-package org
  :ensure t
  :after evil evil-org org-contrib
  :demand t
  :diminish org-indent-mode eldoc-mode auto-revert-mode
  ; Enable word wrap and org indenting
  :hook ((org-mode . toggle-truncate-lines)
         (org-mode . org-indent-mode))
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map org-mode-map
         ("C-c ," . org-time-stamp-inactive)
         ("C-c ." . org-time-stamp)
         ("C-c x" . org-cut-subtree)
         ("S-<return>" . evil-org-open-below))
  :mode (("\\.org\\'" . org-mode)
         ("\\.org$" . org-mode))
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
        ;; Theming
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-tags-column 0
        org-auto-align-tags nil
        org-agenda-tags-column 0
        org-catch-invisible-edits 'show-and-error
        org-special-ctrl-a/e t
        org-insert-heading-respect-content t
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
                            (and (not (string-match "/archive/"(file-name-directory x))) (not (string-match "/denote/"(file-name-directory x)))))
                          (directory-files-recursively org-directory "\\.org$"))
        org-archive-location (concat "%s_archive::" org-directory "/archive")
        org-tag-persistent-alist '((:startgroup . nil)
                                   ("important" . ?i)
                                   ("backlog" . ?x)
                                   (:endgroup . nil)
                                   ("ARCHIVE" . ?A)
                                   (:startgroup . nil)
                                   ("@Work" . ?w)
                                   ("@Home" . ?h)
                                   ("@Runescape" . ?r)
                                   ("@Personal" . ?p)
                                   (:endgroup . nil))
        org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CLOSED"))
        org-capture-templates '(("t" "Inbox" entry (file+headline org-default-notes-file "Inbox")
                                 "* TODO %?\n:PROPERTIES:\n:CREATION: %U\n:END:\n")
                                ("w" "Work Task" entry (file+headline org-work-file "Inbox")
                                 "* TODO %?\n:PROPERTIES:\n:CREATION: %U\n:TRACKZILLA: N/A\n:END:\n")
                                ("i" "Work Issue" entry (file+headline org-work-file "Inbox")
                                 (concat "* TODO %?\n"
                                         ":PROPERTIES:\n:CREATION: %U\n:TRACKZILLA: N/A\n:END:\n"
                                         "#+TITLE: Issue comment\n"
                                         "#+BEGIN_SRC markdown :hidden\n"
                                         "#+END_SRC\n"))
                                ("h" "Home Task" entry (file+headline org-home-file "Inbox")
                                 "* TODO %?\n:PROPERTIES:\n:CREATION: %U\n:END:\n")
                                ("a" "App Task" entry (file+headline org-app-file "Inbox")
                                 "* TODO %?\n:PROPERTIES:\n:CREATION: %U\n:END:\n")
                                ("j" "Journal" entry (file+datetree org-default-notes-file)
                                 "* %?\n%U\n%a")
                                ("n" "Link Inbox" entry (file+headline org-default-notes-file "Inbox")
                                 "* TODO %?\n:PROPERTIES:\n:CREATION: %U\n:END:\n%a")
                                ("o" "Link Work Issue" entry (file+headline org-work-file "Inbox")
                                 "* TODO %?\n:PROPERTIES:\n:CREATION: %U\n:END:\n%a")
                                ("m" "Link Home Task" entry (file+headline org-home-file "Tasks")
                                 "* TODO %?\n:PROPERTIES:\n:CREATION: %U\n:END:\n%a")))
  :config
  ; Shamelessly stolen from https://emacs.stackexchange.com/questions/44914/choose-individual-startup-visibility-of-org-modes-source-blocks
  ; This code lets you put :hidden on an org code block to hide it by default
  (defun individual-visibility-source-blocks ()
    "Fold some blocks in the current buffer."
    (interactive)
    (org-show-block-all)
    (org-block-map
     (lambda ()
       (let ((case-fold-search t))
         (when (and
                (save-excursion
                  (beginning-of-line 1)
                  (looking-at org-block-regexp))
                (cl-assoc
                 ':hidden
                 (cl-third
                  (org-babel-get-src-block-info))))
           (org-hide-block-toggle))))))
  (add-hook 'org-mode-hook (function individual-visibility-source-blocks))
  (appt-activate 1)
  (require 'org-checklist))

; Denote settings
(use-package denote
  :ensure t
  :after evil org
  :demand t
  ; Make denote links work
  :hook ((find-file . denote-link-buttonize-buffer)
         ; Denote formatting for files in dired
         (dired-mode . denote-dired-mode))
  :bind (("C-c n n" . denote)
         ("C-c n r" . denote-rename-file)
         ("C-c n l" . denote-link)
         ("C-c n t" . denote-type)
         ("C-c n f" . denote-rename-file-using-front-matter))
  :init
  ; Copied from org settings
  (if (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
      (setq org-directory '"C:/org/org-notes")
      (setq org-directory '"~/Documents/GitHub/org-notes"))
  (setq denote-directory (concat org-directory "/denote")
        ; Eventually I want template added here, need to define denote-templates
        denote-prompts '(subdirectory title keywords)
        denote-file-type 'org
        denote-known-keywords '("emacs" "work" "reflection" "denote" "politics" "philosophy" "recipe" "discussion")
        denote-date-prompt-use-org-read-date t)
  :config
  (evil-define-key 'normal 'global
    (kbd "SPC n n") 'denote
    (kbd "SPC n r") 'denote-rename-file
    (kbd "SPC n l") 'denote-link
    (kbd "SPC n t") 'denote-type
    (kbd "SPC n f") 'denote-rename-file-using-front-matter))
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
(use-package org-super-agenda
  :ensure t
  :demand t
  :after org
  :init
  (setq org-super-agenda-groups
        '((:name "Personal Tasks"
                 :tag "@Home"
                 :tag "@Runescape"
                 :tag "@Personal"
                 :order 3)
          (:name "Schedule"
                 :time-grid t
                 :todo "IN-PROGRESS"
                 :order 0)
          (:name "Important"
                 :tag "important"
                 :priority "A"
                 :order 1)
          (:name "High-Priority"
                 :priority "B"
                 :order 2)
          (:name "Medium-Priority"
                 :priority "C"
                 :order 4)))
  :config
  (org-super-agenda-mode))
(use-package evil-org
  :ensure t
  :demand t
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
(use-package sql
  :hook ((sql-mode . sql-highlight-mariadb-keywords)
         (sql-interactive-mode . (lambda () (toggle-truncate-lines t))))
  :init
  (setq sql-mysql-options '("--prompt=mysql> " "-C" "-t" "-f" "-n")))

;; eshell
(use-package eshell
  :bind ("C-x e" . eshell)
  :hook (eshell-mode . (lambda ()
                         ; Quick switch to org
                         (eshell/alias "so"
                                       (if (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
                                           "cd C:/org/org-notes"
                                           "cd ~/Documents/GitHub/org-notes"))
                         ; Opening files, these open in-frame
                         (eshell/alias "e" "find-file $1")
                         (eshell/alias "ff" "find-file $1")
                         (eshell/alias "emacs" "find-file $1")
                         ; These open files in a separate frame
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
                           ; This is here because it uses the ls variable set above
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
