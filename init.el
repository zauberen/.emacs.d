;;; init.el --- EMACS config -*- lexical-binding: t -*-
;;; Commentary:
;;; My Emacs configuration!
;;; Code:

;; Default to utf-8
(set-default-coding-systems 'utf-8)

;; Package signatures are broken in Windows
(when (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
  (setq package-check-signature nil))

;; Package management ;;
(setq package-native-compile t
      package-quickstart t)
(load-file (expand-file-name "elpaca.el" user-emacs-directory))

;;; General configuration
(use-package emacs
  :ensure nil
  :custom
  (ispell-dictionary "en_US")
  :config
  ;; Kill buffer command
  (global-set-key (kbd "C-x k") 'kill-current-buffer)
  ;; Indent region command
  (global-set-key (kbd "C-c i") 'indent-region))
;; Line numbering and word wrap
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)
(column-number-mode)
(transient-mark-mode 1)
(global-visual-line-mode)
;; Mac font sizing is unusually small
;(if (eq system-type 'darwin)
    ;; 170 when doing presentations
    ;; 130-135 when using mac screen
    ;; 120 when using 1080p screen on mac
    ;(setq font-height '135)
  ;(setq font-height '100))
(setq use-default-font-for-symbols nil)
;; Set display of menu according with whether GUI is used
(when (and (or (eq system-type 'ms-dos) (eq system-type 'windows-nt)) (display-graphic-p))
  (menu-bar-mode 1))

;; Fonts
;(set-face-attribute 'default nil :height font-height :family "Hack")
;(set-face-attribute 'fixed-pitch nil :family "Hack")
;(set-face-attribute 'variable-pitch nil :family "Hack")
;; Use fontaine-set-preset to change settings.
(use-package fontaine
  :ensure t
  :demand t
  :config
  ;; FIXME: Use new fontaine-mode.
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
  (fontaine-mode 1)
  ;; NOTE: Since Fontaine doesn't support the :width attribute, I
  ;; don't use it for tab-bar faces. See
  ;; <https://github.com/protesilaos/fontaine/issues/6>.
  ;; FIXME: Fontaine 2.1 supports the :width attribute.
  (cl-callf2 remq 'tab-bar fontaine-faces)
  :custom
  (fontaine-presets
   '((t :default-family "Hack"
        :default-weight normal
        :default-height 100
        :fixed-pitch-height 1.0
        :fixed-pitch-serif-height 1.0
        :variable-pitch-family "Hack"
        :variable-pitch-height 1.0
        :bold-weight bold
        :italic-slant italic
        :line-spacing nil
        :fixed-pitch-family nil
        :fixed-pitch-weight nil
        :fixed-pitch-serif-family nil
        :fixed-pitch-serif-weight nil
        :variable-pitch-weight nil
        :bold-family nil
        :italic-family nil)
     (regular :default-height 100)
     (medium :default-weight semilight
             :default-height 140
             :bold-weight extrabold)
     (large :default-weight semilight
            :default-height 170
            :bold-weight extrabold)

     (hack-ttf :default-family "Hack"
               :default-height 100
               :default-weight normal)
     (hack-ttf-mac :default-family "Hack"
                   :default-height 135
                   :default-weight normal)
     (hack-ttf-big :default-family "Hack"
                   :default-height 170
                   :default-weight normal))))

(load-file (expand-file-name "tweaks.el" user-emacs-directory))

;; Diminish to hide minor modes
;; Not necessary with doom modeline but will be useful if I drop it in the future
(use-package diminish
  :ensure t)

;; Icons and emoji support and it's cross platform
;; Must run `all-the-icons-install-fonts' on first load!
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))
(use-package all-the-icons-completion
  :ensure t
  :after all-the-icons
  :if (display-graphic-p)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :config
  (all-the-icons-completion-mode))

(use-package all-the-icons-dired
  :ensure t
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode))

;; Base ripgrep search outside of minibuffer
(use-package deadgrep
  :ensure t
  :demand t
  :bind ("C-M-s" . deadgrep))
;; Allows editing directly inside the deadgrep buffer
(use-package wgrep-deadgrep
  :ensure t
  :demand t)
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

;; Scrollbar replacement
;; (use-package yascroll
;;   :ensure t :demand t
;;   :init
;;   (scroll-bar-mode -1)
;;   :config
;;   (global-yascroll-bar-mode 1))
;; More stable but less professional scrollbar replacement
(use-package nyan-mode
  :ensure t :demand t
  :init
  (scroll-bar-mode -1)
  :hook (window-state-change . (lambda () (setq-local nyan-bar-length
                                                      (max 10
                                                           (/ (window-width)
                                                              5)))))
  :config
  (setq nyan-bar-length 25))

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
  (load-theme (cond
               ((eq system-type 'darwin) 'doom-gruvbox)
               ((or (eq system-type 'ms-dos) (eq system-type 'windows-nt)) 'doom-molokai)
               (t 'doom-Iosvkem))
              t)
  :ensure t)
(use-package doom-modeline
  :ensure t
  :init
  ;; Display all-the-icons icons when able to (display-graphic-p)
  (if (display-graphic-p)
      (setq doom-modeline-icon t
            doom-modeline-time t
            doom-modeline-project-detection nil
            doom-modeline-height (+ (frame-char-height) 5)
            display-time-default-load-average nil)
    (setq doom-modeline-icon nil
          doom-modeline-time t
          doom-modeline-height 17
          display-time-default-load-average nil))
  (doom-modeline-mode 1))

;; Load exwm only if it is already installed
(if (elpaca-installed-p 'exwm)
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
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; New formatting engine copied from doom, see https://editorconfig.org
;(use-package editorconfig
  ;:ensure t
  ;:demand t
  ;:config
  ;(editorconfig-mode 1))

;; Jump to text with avy
(use-package avy
  :ensure t
  :demand t
  :bind ("C-l" . avy-goto-line))

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
  :demand t
  :after evil
  :diminish projectile-mode
  :bind (:map projectile-mode-map
         ("C-c p" . projectile-command-map))
  :init
  (when (not (or (eq system-type 'ms-dos) (eq system-type 'windows-nt)))
    (setq projectile-project-search-path '("~/.emacs.d"
                                            ("~/Documents/GitHub" . 1))))
  :config
  (projectile-mode +1)
  ; Bind the most useful projectile commands to easier keys
  (evil-define-key 'normal 'global
    ; Note that f is overridden if minibuffer.el is included to use consult-projectile
    (kbd "SPC f") #'projectile-find-file
    (kbd "SPC p") #'projectile-switch-project
    (kbd "SPC e") #'projectile-run-eshell)

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

;; Set org directory, set outside of any main package becauses this is referenced in several places.
(if (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
    (setq org-directory '"C:/org/org-notes")
  (setq org-directory '"~/Documents/GitHub/org-notes"))


(load-file (expand-file-name "snippets.el" user-emacs-directory))
(load-file (expand-file-name "hydra.el" user-emacs-directory))
(load-file (expand-file-name "completions.el" user-emacs-directory))
(load-file (expand-file-name "minibuffer.el" user-emacs-directory))
(load-file (expand-file-name "corfu.el" user-emacs-directory))


;; Language configuration
(load-file (expand-file-name "lang.el" user-emacs-directory))

;; Org mode
(load-file (expand-file-name "org.el" user-emacs-directory))

;; Obsidian in Emacs configuration
(load-file (expand-file-name "obsidian.el" user-emacs-directory))

;; Text editor functionality (bibliography, etc)
(load-file (expand-file-name "editor.el" user-emacs-directory))

;; Eshell configuration
(load-file (expand-file-name "eshell.el" user-emacs-directory))

;; AI integration
(load-file (expand-file-name "ai.el" user-emacs-directory))

;; Fun stuff
(load-file (expand-file-name "fun.el" user-emacs-directory))

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

;; Wait for everything to be loaded
(elpaca-wait)

;; Load custom settings
(let ((local-settings (expand-file-name "local.el" user-emacs-directory)))
  (when (file-exists-p local-settings)
    (load-file local-settings)))

;; Open init.el on opening
(find-file (expand-file-name "init.el" user-emacs-directory))

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
