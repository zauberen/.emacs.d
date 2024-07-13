;;; init.el --- EMACS config
;;; Commentary:
;;; My Emacs configuration!
;;; Code:

;; Default to utf-8
(set-default-coding-systems 'utf-8)

;; Package management ;;
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Uncomment for systems which cannot create symlinks:
;; (elpaca-no-symlink-mode)

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

;;; Non-use-package configuration
;; Line numbering and word wrap
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)
(column-number-mode)
(transient-mark-mode 1)
(global-visual-line-mode)
;; Mac font sizing is unusually small
(if (eq system-type 'darwin)
    ; 170 when doing presentations
    ; 130 when using mac screen
    (setq font-height '130)
    (setq font-height '100))
(setq use-default-font-for-symbols nil)
(set-face-attribute 'default nil :height font-height :family "Hack")
(set-face-attribute 'fixed-pitch nil :family "Hack")
(set-face-attribute 'variable-pitch nil :family "Hack")
;; Set display of menu according with whether GUI is used
(when (and (or (eq system-type 'ms-dos) (eq system-type 'windows-nt)) (display-graphic-p))
  (menu-bar-mode 1))

(load-file (expand-file-name "tweaks.el" user-emacs-directory))

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
;; C-c d contains context limited replace all functions (defun scope, below or above cursor)
(use-package substitute
  :ensure t
  :demand t
  :after evil
  :bind (("C-d" . substitute-target-in-buffer)
         ("C-c d" . #'substitute-prefix-map))
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
  (doom-modeline-mode 1))

;; Load exwm only if it is already installed
(if (package-installed-p 'exwm)
   (load-file (expand-file-name "exwm.el" user-emacs-directory)))

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
  :bind ("C-l" . avy-goto-line))

;; Workspaces with persp-mode
;; Removed because it doesn't actually work, it's more of a stash of buffers
;(use-package persp-mode
  ;;:hook (window-setup . #'(lambda () (persp-mode 1)))
  ;:init
  ;(setq persp-keymap-prefix (kbd "C-c d"))
  ;:config
  ;(setq wg-morph-on nil)
  ;(setq persp-autokill-buffer-on-remove 'kill-weak)
  ;(persp-mode 1))

;; Magit and other git plugins
(load-file (expand-file-name "git.el" user-emacs-directory))

(load-file (expand-file-name "evil.el" user-emacs-directory))

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
  :init
  (setq orderless-component-separator #'orderless-escapable-split-on-space
        ;orderless-style-dispatchers '(+orderless-dispatch)
        completion-category-defaults nil
        ; I had this nil before
        completion-category-overrides '((file (styles . (partial-completion))))
        completion-styles '(orderless partial-completion basic)))

(load-file (expand-file-name "snippets.el" user-emacs-directory))
(load-file (expand-file-name "hydra.el" user-emacs-directory))
(load-file (expand-file-name "completions.el" user-emacs-directory))
(load-file (expand-file-name "minibuffer.el" user-emacs-directory))
(load-file (expand-file-name "corfu.el" user-emacs-directory))


;; Language configuration
(load-file (expand-file-name "web.el" user-emacs-directory))
(load-file (expand-file-name "lang.el" user-emacs-directory))

;; Org mode
(load-file (expand-file-name "org.el" user-emacs-directory))

;; Obsidian in Emacs configuration
(load-file (expand-file-name "obsidian.el" user-emacs-directory))

;; Text editor functionality (bibliography, etc)
(load-file (expand-file-name "editor.el" user-emacs-directory))

;; Eshell configuration
(load-file (expand-file-name "eshell.el" user-emacs-directory))

;; Save minibuffer history
(use-package savehist
  :config
  (add-to-list 'savehist-additional-variables 'vertico-repeat-history)
  (savehist-mode))

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
(let ((local-settings (expand-file-name "local.el" user-emacs-directory)))
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
