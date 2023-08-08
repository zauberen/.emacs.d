;;; init.el --- EMACS config
;;; Commentary:
;;; My Emacs configuration!
;;; Code:

;; Package signatures are broken in Windows
(if (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
    (setq package-check-signature nil
          gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
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
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(setq package-archive-priorities '(("gnu" . 20)
                                   ("nongnu" . 10)
                                   ("melpa" . 30)
                                   ("melpa-stable" . 40)))
(package-initialize)
(eval-when-compile
  (package-install-selected-packages t)
  (require 'use-package))
;(setq use-package-always-pin '"melpa-stable")

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

(load-file "~/.emacs.d/tweaks.el")

;; Kill buffer command
(global-set-key (kbd "C-x k") 'kill-current-buffer)
;; Indent region command
(global-set-key (kbd "C-c i") 'indent-region)

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
  :demand t
  :ensure-system-package ag)
(use-package wgrep-ag
  :demand t
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

;; Basic error checking with flycheck
(use-package flycheck
  :ensure t
  :pin melpa-stable
  :demand t
  :config
  (global-flycheck-mode))

;; New formatting engine copied from doom, see https://editorconfig.org
(use-package editorconfig
  :ensure t
  :demand t
  :config
  (editorconfig-mode 1))

;; Jump to text with avy
(use-package avy
  :ensure t
  :demand t
  :bind ("C-a" . avy-goto-subword-1))

;; Magit and other git plugins
(load-file "~/.emacs.d/git.el")

(load-file "~/.emacs.d/evil.el")

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
    ; Note that f is overridden if minibuffer.el is included to use consult-projectile
    (kbd "SPC f") #'projectile-find-file
    (kbd "SPC p") #'projectile-switch-project
    (kbd "SPC e") #'projectile-run-eshell
    ; Also bind ag
    (kbd "SPC a a") #'ag
    (kbd "SPC a p") #'projectile-ag))

;; Virtico, Corfu, cape, orderless, consult, embark, marginalia
(use-package orderless
  :ensure t
  :pin melpa
  :init
  (setq orderless-component-separator #'orderless-escapable-split-on-space
        ;orderless-style-dispatchers '(+orderless-dispatch)
        completion-category-defaults nil
        ; I had this nil before
        completion-category-overrides '((file (styles . (partial-completion))))
        completion-styles '(orderless partial-completion basic)))
(load-file "~/.emacs.d/completions.el")
(load-file "~/.emacs.d/minibuffer.el")
(load-file "~/.emacs.d/corfu.el")
(load-file "~/.emacs.d/snippets.el")


;; Language configuration
(load-file "~/.emacs.d/web.el")
(load-file "~/.emacs.d/lang.el")

;; Org mode
(load-file "~/.emacs.d/org.el")

;; Obsidian in Emacs configuration
(load-file "~/.emacs.d/obsidian.el")


;; Eshell configuration
(load-file "~/.emacs.d/eshell.el")

;; Garbage collection
(use-package gcmh
  :ensure t
  :pin melpa
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
