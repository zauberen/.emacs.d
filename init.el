;;; init.el --- EMACS config v01302023
;;; Commentary:
;;; Updates for corfu doc references
;;; Code:
;; Set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             '("org" . "http://orgmode.org/elpa/"))

;; Package signatures are broken in Windows
(if (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
    (setq package-check-signature nil
          gnutls-algorithm-priority "NORMAL")
  (setq network-security-level 'high
        gnutls-verify-error t
        gnutls-min-prime-bits 3072
        gnutls-algorithm-priority "PFS:-VERS-TLS1.2:-VERS-TLS1.1:-VERS-TLS1.0"))

;; Package management ;;
(setq package-selected-packages
      '( evil evil-collection evil-mc which-key
         corfu corfu-terminal cape savehist vertico orderless marginalia consult
         tree-sitter tree-sitter-langs
         lsp-mode lsp-ui consult-lsp
         citre citre-config avy
         yasnippet yasnippet-snippets smartparens evil-smartparens
         embark embark-consult
         ;; Requires svg support in emacs
         ;; svg-lib kind-icon
         org org-contrib org-plus-contrib org-journal evil-org denote
         obsidian
         doom-modeline doom-themes adaptive-wrap editorconfig
         magit magit-todos hl-todo vterm git-gutter
         csv-mode markdown-mode clang-format cmake-mode rust-mode cargo
         flycheck consult-flycheck
         projectile gcmh diminish ag wgrep-ag
         jabber
         gnu-elpa-keyring-update
         monokai-theme))
(setq package-native-compile t
      native-comp-async-report-warnings-errors nil
      package-quickstart t
      vterm-always-compile-module t)
(unless (seq-every-p #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (package-install-selected-packages t))

;; Theming ;;
(load-theme 'doom-molokai t)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)
(column-number-mode)
(global-visual-line-mode)
(setq adaptive-wrap-prefix-mode t)
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
;; Terminal specific settings for corfu
(unless (display-graphic-p)
  (require 'corfu-terminal)
  (corfu-terminal-mode +1))
;; Remove minor modes from mode line
(require 'diminish)
(require 'doom-modeline)
(doom-modeline-mode 1)
(setq doom-modeline-icon nil
      doom-modeline-height 17)
;; smart parens
(require 'smartparens-config)
(smartparens-global-mode 1)
(smartparens-strict-mode 1)
(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
(global-set-key (kbd "C-c (") 'sp-wrap-round)
(global-set-key (kbd "C-c {") 'sp-wrap-curly)
;; magit
(global-git-gutter-mode t)
(custom-set-variables '(git-gutter:update-interval 2))

(custom-set-variables '(git-gutter:modified-sign " ")
                      '(git-gutter:added-sign " ")
                      '(git-gutter:deleted-sign " "))
(set-face-background 'git-gutter:modified "purple")
(set-face-background 'git-gutter:added "green")
(set-face-background 'git-gutter:deleted "red")

(global-set-key (kbd "C-x k") 'kill-current-buffer)

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
;(setq indent-line-function 'insert-tab)
(setq sentence-end-double-space nil)
;; New formatting engine from doom, see https://editorconfig.org
(editorconfig-mode 1)
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
(diminish 'evil-collection-unimpaired-mode)
;; Some quick keybinds
(evil-define-key 'normal 'global
  (kbd "SPC b") #'switch-to-buffer)
;; avy bindings
(evil-define-key 'normal 'global
  (kbd "SPC SPC") #'evil-avy-goto-word-or-subword-1
  (kbd "SPC s") #'evil-avy-goto-char-timer)
;; Allows redo functionality in evil
;; Only works in emacs 28 and later
(evil-set-undo-system 'undo-redo)
(require 'evil-mc)
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
;(defvar evil-mc-mode-line-prefix "mc"
  ;"Override of the default mode line string for `evil-mc-mode'.")
(global-evil-mc-mode 1)
(evil-define-key 'visual evil-mc-key-map
  "A" #'evil-mc-make-cursor-in-visual-selection-end
  "I" #'evil-mc-make-cursor-in-visual-selection-beg)

;; Which key
(require 'which-key)
(which-key-mode)
(diminish 'which-key-mode)
;; Projectile
;; Set up projectile project directories in local.el!
(projectile-mode +1)
(diminish 'projectile-mode)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
; Bind the most useful projectile commands to easier keys
(evil-define-key 'normal 'global
  (kbd "SPC f") #'projectile-find-file
  (kbd "SPC a") #'projectile-ag
  (kbd "SPC p") #'projectile-switch-project
  (kbd "SPC e") #'projectile-run-eshell)

;; ctags setup (citre)
(require 'citre)
(require 'citre-config)
(setq citre-default-create-tags-file-location 'global-cache
      citre-project-root-function #'projectile-project-root
      citre-prompt-language-for-ctags-command t
      citre-use-project-root-when-creating-tags t)
      ;citre-auto-enable-citre-mode-modes '(prog-mode)) ; Commented because it is not needed?
(global-set-key (kbd "C-c t j") 'citre-jump)
(global-set-key (kbd "C-c t J") 'citre-jump-back)
(global-set-key (kbd "C-c t p") 'citre-ace-peek)
(global-set-key (kbd "C-c t u") 'citre-update-this-tags-file)
(evil-define-key 'normal 'citre-mode-map
  (kbd "g d") 'citre-jump
  (kbd "g D") 'citre-jump-back
  (kbd "g p") 'citre-peek
  (kbd "g P") 'citre-ace-peek
  (kbd "SPC u") 'citre-update-this-tags-file)

;; Virtico, Corfu, cape, orderless, consult, embark, marginalia
;;(require 'kind-icon)
(require 'consult)
(evil-define-key 'normal 'global
  (kbd "SPC j") #'evil-collection-consult-jump-list)
(setq read-extended-command-predicate #'command-completion-default-include-p
      completion-styles '(orderless partial-completion basic)
      completion-category-defaults nil
      completion-category-overrides nil
      completion-in-region-function #'consult-completion-in-region
      orderless-component-separator #'orderless-escapable-split-on-space
      cape-dabbrev-min-length 2
      corfu-auto t
      corfu-auto-prefix 1
      ;; corfu-margin-formatters '(kind-icon-margin-formatter)
      ;; kind-icon-default-face 'corfu-default
      ;; kind-icon-blend-background nil
      ;; kind-icon-default-style (plist-put kind-icon-default-style ':height 0.75)
      )

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
                                                         #'cape-ispell))))
(add-hook 'corfu-mode-hook #'corfu-popupinfo-mode)
(vertico-mode)
(marginalia-mode)
(global-corfu-mode)
(require 'embark)
(define-key corfu-map (kbd "RET") nil)
(define-key corfu-map (kbd "M-p") #'corfu-popupinfo-scroll-down)
(define-key corfu-map (kbd "M-n") #'corfu-popupinfo-scroll-up)

;; eglot
;(setq eglot-stay-out-of '(eldoc-documentation-strategy)
      ;eglot-autoshutdown t)
;(advice-add #'eglot-completion-at-point
            ;:before-until #'inside-program-text-p)
;(defun setup-eglot ()
  ;"Enable eglot and its dependencies."
  ;(require 'eglot)
  ;(add-hook 'hack-local-variables-hook #'eglot-ensure nil t))

;; Fancier syntax highlights
(require 'tree-sitter)
(require 'tree-sitter-langs)
(global-tree-sitter-mode)
(diminish 'tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;; Snippets
(require 'yasnippet)
(yas-global-mode 1)
(diminish 'yas-minor-mode)

;; lsp-mode
(require 'lsp-mode)
(setq lsp-completion-provider :none)
; Use consult for lsp completions
(define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols)
(setq lsp-keymap-prefix "C-c l")
(defun corfu-lsp-setup ()
  "Enable lsp and its dependencies."
  (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
        '(orderless)))
(add-hook 'lsp-completion-mode #'corfu-lsp-setup)

;; HTML
(add-to-list 'auto-mode-alist '("\\.html$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode))

;; java
;(defun java-setup-lsp ()
  ;"Set up java lsp."
  ;(require 'lsp-java)
  ;(lsp))
; Not working?
;(add-hook 'java-mode-hook #'java-setup-lsp)
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . java-mode))

;; js (removing auto lsp because its slow af on windows)
(if (not (or (eq system-type 'ms-dos) (eq system-type 'windows-nt)))
    (add-hook 'js-mode-hook #'lsp))

;; python (removing auto lsp because its slow af on windows)
(if (not (or (eq system-type 'ms-dos) (eq system-type 'windows-nt)))
    (add-hook 'python-mode-hook #'lsp))

(require 'csv-mode)
(add-hook 'csv-mode-hook #'csv-align-mode)

;; rust
(setq rust-format-on-save nil)
;(with-eval-after-load 'eglot
  ;(setf (alist-get 'rust-mode eglot-server-programs) '("rust-analyzer"))
  ;(push-default '(rust-analyzer (checkOnSave (command . "clippy")))
                ;eglot-workspace-configuration))
;(add-hook 'rust-mode-hook #'setup-eglot)
(add-hook 'rust-mode-hook #'cargo-minor-mode)
(add-hook 'toml-mode-hook #'cargo-minor-mode)
(with-eval-after-load 'cargo
  (hide-minor-mode 'cargo-minor-mode))
(dolist (sym '(rust-enable-format-on-save rust-disable-format-on-save))
  (put sym 'completion-predicate #'ignore))

;; pascal, innosetup
(add-to-list 'auto-mode-alist '("\\.iss\\'" . pascal-mode))

;; Diary config
(setq view-diary-entries-initially t
      view-calendar-holidays-initially t
      mark-diary-entries-in-calendar t
      mark-holidays-in-calendar t
      appt-display-mode-line t)
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)
(add-hook 'diary-display-hook 'fancy-diary-display)
(add-hook 'list-diary-entries-hook 'sort-diary-entries)
(add-hook 'list-diary-entries-hook 'include-other-diary-files)
(add-hook 'mark-diary-entries-hook 'mark-included-diary-files)
(global-set-key (kbd "C-x c") #'calendar)
(global-set-key (kbd "C-x y") #'diary)

;; Org mode config x denote config
(transient-mark-mode 1)
(require 'org)
;(require 'denote)
;; Diminish org minor modes
(diminish 'org-indent-mode)
(diminish 'eldoc-mode)
(diminish 'auto-revert-mode)
;; Use org mode in org files
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
;; TODO keywords in org
(setq hl-todo-keyword-faces
	'(("TODO"           . "#FF0000")
	  ("IN-PROGRESS"    . "#A020F0")
	  ("WAITING"        . "#FF4500")
	  ("DONE"           . "#1E90FF")
      ("CLOSED"         . "#1E90FF")))
(global-hl-todo-mode)
(setq org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CLOSED")))
;; Archive when closed
(setq org-todo-state-tags-triggers '(("CLOSED" ("ARCHIVE" . t))))
;; Set org directory
(if (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
    (setq org-directory '"C:/org/org-notes")
    (setq org-directory '"~/Documents/GitHub/org-notes"))
(setq diary-file (concat org-directory "/diary"))
(setq appt-display-diary nil)
(appt-activate 1)
; Make list completion make sense
(setq org-enforce-todo-dependencies t
      org-enfocre-todo-checkbox-dependencies t
      org-hide-emphasis-markers t
      ;; Follow links on RET (cannot always use gx)
      org-return-follows-link t
      ;; Make org look better
      org-hide-leading-stars t
      ;; When a todo is set to done, add the completion time
      org-log-done 'note
      ;; Include diary stuff in the org agenda
      org-agenda-include-diary t
      ;; Journal settings
      org-journal-dir (concat org-directory "/journal")
      org-journal-prefix-key "C-c j "
      org-journal-enable-agenda-integration t
      org-journal-file-format "%Y-%m.org"
      org-journal-file-type 'monthly
      ;; Set default directories, files
      org-default-notes-file (concat org-directory "/inbox.org")
      org-work-file (concat org-directory "/org-private/work.org")
      org-home-file (concat org-directory "/home.org")
      org-app-file (concat org-directory "/org-private/dnd-app.org")
      org-agenda-files (seq-filter
                        (lambda(x)
                          (not (string-match "/archive/"(file-name-directory x))))
                        (directory-files-recursively org-directory "\\.org$"))
      org-archive-location (concat "%s_archive::" org-directory "/archive"))
; Denote settings
(setq denote-directory (expand-file-name "denote" org-directory)
      denote-prompts '(title keywords)
      denote-file-type 'org
      denote-known-keywords '("emacs" "work" "reflection" "denote" "politics" "philosophy" "recipe")
      denote-date-prompt-use-org-read-date t)
(evil-define-key 'normal 'global
  (kbd "SPC n n") 'denote
  (kbd "SPC n r") 'denote-rename-file
  (kbd "SPC n l") 'denote-link)
; Load org extensions
(require 'org-journal)
(require 'org-checklist)
(require 'evil-org)
(evil-org-set-key-theme '(navigation insert textobjects additional calendar))
(require 'evil-org-agenda)
(evil-org-agenda-set-keys)
; Diminish org minor modes
(diminish 'evil-org-mode)
(diminish 'org-indent-mode)
; Enable word wrap and org indenting
(add-hook 'org-mode-hook 'toggle-truncate-lines)
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'evil-org-mode)
; Denote formatting for files in dired
(add-hook 'dired-mode-hook #'denote-dired-mode)
; Org capture templates
(setq org-capture-templates
      '(("t" "Inbox" entry (file+headline org-default-notes-file "Inbox")
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
;; Org keybinds
(define-key org-mode-map (kbd "C-c ,") 'org-time-stamp-inactive)
(define-key org-mode-map (kbd "C-c x") 'org-cut-subtree)
(define-key org-mode-map (kbd "S-<return>") 'evil-org-open-below)
(evil-define-key 'normal 'org-mode-map
  (kbd "SPC i") 'org-clock-in)
(evil-define-key 'normal 'org-mode-map
  (kbd "SPC o") 'org-clock-out)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c j j") 'org-journal-new-entry)

;; Obsidian
(require 'obsidian)
(setq obsidian-inbox-directory "Inbox"
      obsidian-path (if (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
                        "C:/Users/dillona/Documents/projects/0notes/Notes/Notes"
                        "~/Documents/Obsidian/notes"))
(obsidian-specify-path obsidian-path)
(define-key obsidian-mode-map (kbd "C-c C-l") 'obsidian-insert-wikilink)
(define-key obsidian-mode-map (kbd "C-<return>") 'obsidian-follow-link-at-point)
(global-set-key (kbd "C-c o") 'obsidian-search)
(global-obsidian-mode t)

;; SQL
(setq sql-mysql-options '("--prompt=mysql> " "-C" "-t" "-f" "-n"))
(add-hook 'sql-mode-hook (lambda ()
                           (require 'sql)
                           (sql-highlight-mariadb-keywords)))
(add-hook 'sql-interactive-mode-hook (lambda () (toggle-truncate-lines t)))

;; eshell
(global-set-key (kbd "C-x e") #'eshell)
; Default shortcuts
(add-hook 'eshell-mode-hook (lambda ()
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
      (eshell/alias "ll" (concat ls " -AlohG --color=always")))))


;; Load custom settings
(let ((local-settings "~/.emacs.d/local.el"))
  (when (file-exists-p local-settings)
    (load-file local-settings)))

;; Open init.el on opening
(set-register ?e (find-file (or user-init-file "")))

;; Open org index if it's on this system
(when (file-exists-p org-default-notes-file)
  (set-register ?f (find-file (or org-default-notes-file ""))))

;; Display this week's agenda
(org-agenda-list)

;; Garbage collection
(setq gcmh-idle-delay 'auto
      gcmh-auto-idle-delay-factor 10
      gcmh-high-cons-threshold (* 32 1024 1024))
(gcmh-mode 1)
(diminish 'gcmh-mode)



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
