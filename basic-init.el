;;; basic-init.el --- EMACS config v08262022
;;; Commentary:
;;; Building basics still
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
(package-initialize)
(package-refresh-contents)

;; Package management ;;
(setq package-selected-packages
      '( evil evil-collection
	 ;; corfu corfu-doc cape kind-icon vertico orderless marginalia consult
         flycheck
         counsel projectile
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
(global-set-key ["C-x k"] 'kill-current-buffer)
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
              tab-always-indent nil
              require-final-newline t)
(setq sentence-end-double-space nil)
;; Scrolling
(setq scroll-conservatively 101
      scroll-margin 0
      next-screen-context-lines 3)
;; Sandbox/suppress auto file creation
;;(setq auto-save-file-name-transforms
      ;;`((".*" ,(file-name-concat user-emacs-directory "auto-save/") t))
      ;;make-backup-files nil
      ;;create-lockfiles nil
      ;;custom-file null-device)
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
;; Fuzzy find ;;
(ivy-mode)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; All of the default keybinds for ivy
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
;; projectile settings
(projectile-mode +1)
;; Recommended keymap prefix on macOS
;; (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;; Recommended keymap prefix on Windows/Linux
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;; Set up projects for fuzzy search
(setq projectile-project-search-path '(
				       "C:/Users/dillona/Documents/projects"
				       ("C:/Git" . 1)))

(set-register ?e (find-file (or user-init-file "")))

;; Editor package management section
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(## evil-collection evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; basic-init.el ends here
