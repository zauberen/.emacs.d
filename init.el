;;; init.el --- EMACS config v08262022
;;; Commentary:
;;; Drop ivy
;;; Code:
;; Set up package.el to work with MELPA
;; Security ;;
(setq network-security-level 'high
      gnutls-verify-error t
      gnutls-min-prime-bits 3072
      gnutls-algorithm-priority "PFS:-VERS-TLS1.2:-VERS-TLS1.1:-VERS-TLS1.0")

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; Package management ;;
(setq package-selected-packages
      '( evil evil-collection which-key
         corfu corfu-doc cape savehist vertico orderless marginalia consult
         ;; Requires svg support in emacs
         ;; svg-lib kind-icon
         magit magit-todos hl-todo vterm
         eglot markdown-mode clang-format cmake-mode rust-mode cargo
         flycheck projectile gcmh
         monokai-theme))
(setq package-native-compile t
      native-comp-async-report-warnings-errors nil
      package-quickstart t
      vterm-always-compile-module t)
(unless (seq-every-p #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (package-install-selected-packages t))

;; Theming ;;
(load-theme 'monokai t)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)
(set-face-attribute 'default nil :height 120 :family "Hack")
(set-face-attribute 'fixed-pitch nil :family "Hack")
(set-face-attribute 'variable-pitch nil :family "Hack")
(global-hl-todo-mode)
;; magit
(setq magit-view-git-manual-method 'man
      transient-history-file null-device
      magit-save-repository-buffers 'dontask
      magit-delete-by-moving-to-trash nil)
(with-eval-after-load 'magit
  (remove-hook 'server-switch-hook #'magit-commit-diff)
  (magit-todos-mode))
;;(tool-bar-mode -1)
;; Hide welcome screen
(setq inhibit-startup-screen t
      initial-scratch-message nil
      server-client-instructions nil)
;; Autocomplete ;;
(global-flycheck-mode)
(setq use-short-answers t
      confirm-kill-processes nil
      kill-buffer-query-functions nil
      auth-source-save-behavior nil
      enable-local-variables :safe
      disabled-command-function nil)
(global-set-key (kbd "C-x k") 'kill-current-buffer)
;; Save minibuffer history
(savehist-mode)
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
(setq indent-line-function 'insert-tab)
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
;; Evil ;;
(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(require 'evil)
;; Activates additinal evil bindings
(when (require 'evil-collection nil t)
    (evil-collection-init))
(evil-mode 1)
;; Allows redo functionality in evil
;; Only works in emacs 28 and later
(evil-set-undo-system 'undo-redo)
;; Which key
(require 'which-key)
(which-key-mode)
;; Projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-project-search-path '(("~/Documents/GitHub" . 1)))
;; Corfu
;;(require 'kind-icon)
(setq read-extended-command-predicate #'command-completion-default-include-p
      completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-in-region-function #'consult-completion-in-region
      orderless-component-separator #'orderless-escapable-split-on-space
      completion-at-point-functions (list #'cape-file
                                          (cape-super-capf #'cape-dabbrev
                                                           #'cape-ispell))
      cape-dabbrev-min-length 3
      corfu-auto t
      corfu-auto-prefix 1
      ;; corfu-margin-formatters '(kind-icon-margin-formatter)
      ;; kind-icon-default-face 'corfu-default
      ;; kind-icon-blend-background nil
      ;; kind-icon-default-style (plist-put kind-icon-default-style ':height 0.75)
      )
(vertico-mode)
(marginalia-mode)
(global-corfu-mode)
(add-hook 'corfu-mode-hook #'corfu-doc-mode)
(define-key corfu-map (kbd "RET") nil)
(define-key corfu-map (kbd "M-p") #'corfu-doc-scroll-down)
(define-key corfu-map (kbd "M-n") #'corfu-doc-scroll-up)
;; eglot
(setq eglot-stay-out-of '(eldoc-documentation-strategy)
      eglot-autoshutdown t)
(advice-add #'eglot-completion-at-point
            :before-until #'inside-program-text-p)
(defun setup-eglot ()
  "Enable eglot and its dependencies."
  (require 'eglot)
  (add-hook 'hack-local-variables-hook #'eglot-ensure nil t))

;; python
(add-hook 'python-mode-hook #'setup-eglot)
(defun ipython ()
  "Run ipython in vterm."
  (interactive)
  (defvar vterm-shell)
  (let ((vterm-shell "ipython"))
    (vterm-other-window)))
;; rust
(setq rust-format-on-save nil)
(with-eval-after-load 'eglot
  (setf (alist-get 'rust-mode eglot-server-programs) '("rust-analyzer"))
  (push-default '(rust-analyzer (checkOnSave (command . "clippy")))
                eglot-workspace-configuration))
(add-hook 'rust-mode-hook #'setup-eglot)
(add-hook 'rust-mode-hook #'cargo-minor-mode)
(add-hook 'toml-mode-hook #'cargo-minor-mode)
(with-eval-after-load 'cargo
  (hide-minor-mode 'cargo-minor-mode))
(dolist (sym '(rust-enable-format-on-save rust-disable-format-on-save))
  (put sym 'completion-predicate #'ignore))

;; Diary config
(setq view-diary-entries-initially t
      view-calendar-holidays-initially t
      mark-diary-entries-in-calendar t
      mark-holidays-in-calendar t)
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)
(add-hook 'diary-display-hook 'fancy-diary-display)
(add-hook 'list-diary-entries-hook 'sort-diary-entries)
(add-hook 'list-diary-entries-hook 'include-other-diary-files)
(add-hook 'mark-diary-entries-hook 'mark-included-diary-files)
(global-set-key (kbd "C-x c") #'calendar)
(global-set-key (kbd "C-x y") #'diary)

;; Org mode config
(transient-mark-mode 1)
(require 'org)
;; Use org mode in org files
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
;; TODO keywords in org
(setq org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))
;; Archive when cancelled or done
(setq org-todo-state-tags-triggers '(("CANCELLED" ("ARCHIVE" . t)) ("DONE" ("ARCHIVE" . t))))
;; Org tags
(setq org-tag-persistent-alist '((:startgroup . nil)
                                 ("@work" . ?w)
                                 ("@home" . ?h)
                                 (:endgroup . nil)
                                 ("emacs" . ?e)
                                 ("cuervo-burrito" . ?c)
                                 ("side-project" . ?s)))
;; Set org directory
(if (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
    (setq org-directory '"C:/org/org-notes")
    (setq org-directory '"~/Documents/GitHub/org-notes"))
(setq diary-file (concat org-directory "/diary"))
;; Make list completion make sense
(setq org-enforce-todo-dependencies t
      org-enfocre-todo-checkbox-dependencies t
      ;; Follow links on RET
      org-return-follows-link t
      ;; Make org look better
      org-hide-leading-stars t
      ;; When a todo is set to done, add the completion time
      org-log-done 'time
      ;; Follow links on RET
      org-return-follows-link t
      ;; Include diary stuff in the org agenda
      org-agenda-include-diary t
      ;; Set default directories, files
      org-default-notes-file (concat org-directory "/inbox.org")
      org-agenda-files 'org-directory
      org-archive-location (concat "%s_archive::" org-directory "/archive"))
;; Pretty indenting in org mode
(add-hook 'org-mode-hook 'org-indent-mode)
;; Basic keybinds
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)



;; Open init.el and org inbox on opening
(set-register ?e (find-file (or user-init-file "")))
(set-register ?f (find-file (or org-default-notes-file "")))



;; Garbage collection
(setq gcmh-idle-delay 'auto
      gcmh-auto-idle-delay-factor 10
      gcmh-high-cons-threshold (* 32 1024 1024))
(gcmh-mode 1)

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
