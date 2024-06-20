;;; init.el --- EMACS config
;;; Commentary:
;;; My Emacs configuration!
;;; Code:

;; Default to utf-8
(set-default-coding-systems 'utf-8)

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
      '( use-package use-package-ensure-system-package))
(setq package-native-compile t
      native-comp-async-report-warnings-errors nil
      package-quickstart t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;(setq package-archive-priorities '(("gnu" . 20)
                                   ;("nongnu" . 10)
                                   ;("melpa" . 50)
                                   ;("melpa-stable" . 40)))
(package-initialize)
;; Adds the :vc option to use-package, does not work on windows though
;; Will need to wrap any use-package blocks in windows checks
;(unless (package-installed-p 'vc-use-package)
  ;(package-vc-install "https://github.com/slotThe/vc-use-package"))
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

(load-file "~/.emacs.d/tweaks.el")

;; Kill buffer command
(global-set-key (kbd "C-x k") 'kill-current-buffer)
;; Indent region command
(global-set-key (kbd "C-c i") 'indent-region)

(use-package use-package-ensure-system-package
  :ensure t)
(use-package diminish
  :ensure t)

;; ag, the silver searcher
(use-package ag
  :ensure t
  :demand t)
(use-package wgrep-ag
  :demand t
  :ensure t)
;; Used to replace all instances of the word or selection under the cursor
;; C-c r contains context limited replace all functions (function scope, below or above cursor)
(use-package substitute
  :ensure t
  :demand t
  :after evil
  :bind (("C-d" . substitute-target-in-buffer)
         ("C-c r f" . substitute-target-in-defun)
         ("C-c r a" . substitute-target-above-point)
         ("C-c r b" . substitute-target-below-point))
  :config
  (evil-define-key 'normal 'global
    (kbd "C-d") #'substitute-target-in-buffer)
  (evil-define-key 'visual 'global
    (kbd "C-d") #'substitute-target-in-buffer))

;; Theming ;;
; I really like ef-elea-dark
(use-package ef-themes
  :config
  ;(when (not (or (eq system-type 'ms-dos) (eq system-type 'windows-nt)))
    ;(load-theme 'ef-elea-dark t))
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
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  (if (not (eq system-type 'darwin))
    ; A selection of good light themes: doom-flatwhite, also doom-miramare, and the light gruvbox
    ; Dark themes: doom-gruvbox, doom-molokai, doom-tomorrow-night
    ; While I like the idea, they give me a headache
    (load-theme 'doom-molokai t)
    ; The comments on gruvbox are too good, I need them at work
    ;(load-theme 'doom-gruvbox t)
    ; Use gruvbox at home since it's close but just different enough to give a different context
    (load-theme 'doom-gruvbox t))
  :ensure t)
(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-icon nil                 ; Don't use icons
        doom-modeline-time t                   ; Show the time
        doom-modeline-height 17                ; Reasonable modeline height
        display-time-default-load-average nil) ; Don't show CPU with system time
  ; Unicode fallback looks pretty bad on MacOS
  ; Also bad on wsl, just getting rid of this for now
  ;(when (not (eq system-type 'darwin))
        ;(setq doom-modeline-unicode-fallback t))
  :config
  ;(display-time-mode)
  (doom-modeline-mode 1))

;; Prism, theme enhancement to show depth
;; prism-mode for C like langs (parens/curls) prism-whitespace-mode for python like langs
(use-package prism
  :ensure t)

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
  :bind (("C-a" . avy-goto-subword-1)
         ("C-l" . avy-goto-line)))

;; Workspaces with persp-mode
;; Removed because it doesn't actually work, it's more of a stash of buffers
;(use-package persp-mode
  ;:ensure t
  ;;:hook (window-setup . #'(lambda () (persp-mode 1)))
  ;:init
  ;(setq persp-keymap-prefix (kbd "C-c d"))
  ;:config
  ;(setq wg-morph-on nil)
  ;(setq persp-autokill-buffer-on-remove 'kill-weak)
  ;(persp-mode 1))

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

(load-file "~/.emacs.d/snippets.el")
(load-file "~/.emacs.d/hydra.el")
(load-file "~/.emacs.d/completions.el")
(load-file "~/.emacs.d/minibuffer.el")
(load-file "~/.emacs.d/corfu.el")


;; Language configuration
(load-file "~/.emacs.d/web.el")
(load-file "~/.emacs.d/lang.el")

;; Org mode
(load-file "~/.emacs.d/org.el")

;; Obsidian in Emacs configuration
(load-file "~/.emacs.d/obsidian.el")

;; Text editor functionality (bibliography, etc)
(load-file "~/.emacs.d/editor.el")

;; Eshell configuration
(load-file "~/.emacs.d/eshell.el")

;; Save minibuffer history
(use-package savehist
  :ensure t
  :config
  (add-to-list 'savehist-additional-variables 'vertico-repeat-history)
  (savehist-mode))

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
(find-file (or user-init-file ""))

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
