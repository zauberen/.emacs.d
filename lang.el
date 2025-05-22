;;; lang.el --- Misc language configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Programming language configuration.
;;; Code:

;; Java Configuration
(use-package dap-mode
  :ensure t
  :after lsp-mode
  :functions dap-hydra/nil
  :hook ((dap-mode . dap-ui-mode)
         (dap-mode . dap-tooltip-mode)
         (dap-session-created . (lambda (&_rest) (dap-hydra)))
         (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra)))
         (dap-terminated . (lambda (&_rest) (dap-hydra/nil))))
  :bind (:map lsp-mode-map
              ("M-S-d" . dap-debug)
              ("M-d" . dap-hydra))
  :config
  (require 'dap-java)
  (require 'dap-python))

(use-package dap-java :after dap-mode :ensure nil)

(use-package lsp-java
  :ensure t
  :after dap-mode
  :hook (java-mode . (lambda ()
                       (when (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
                         (citre-use-global-windows))))
  :bind (("C-c t s" . tomcat-start)
         ("C-c t x" . tomcat-stop)
         ("C-c t c" . tomcat-clear-logs))
  :init
  (if (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
      (setq lsp-java-java-path "C:/Program Files/Eclipse Adoptium/jdk-17.0.7.7-hotspot/bin/java.exe"
            lsp-java-configuration-runtimes '[(:name "JavaSE-1.8"
                                                     :path "C:/Program Files/Java/jdk1.8.0_202"
                                                     :default t)
                                              (:name "JavaSE-17"
                                                     :path "C:/Program Files/Eclipse Adoptium/jdk-17.0.7.7-hotspot")])
    (setq lsp-java-java-path "/usr/bin/java"
          lsp-java-configuration-runtimes '[(:name "OpenJDK-21"
                                                   :path "/opt/homebrew/opt/openjdk@21")]))
  ;; current VSCode defaults
  (setq lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx2G" "-Xms100m")
        ;; Default path, change this in local.el!
        tomcat-path "~/tomcat"
        ;; Set the name of the catalina script. If using binary distributions, this should work out of the box.
        tomcat-catalina-name (if (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
                                 "catalina.bat"
                               "catalina.sh"))
  (defun tomcat-clear-logs ()
    "Clears the tomcat log."
    (interactive)
    (with-current-buffer (get-buffer-create "*tomcat*") (erase-buffer)))
  (defun tomcat-start ()
    "Starts tomcat on the configured tomcat-path."
    (interactive)
    (tomcat-clear-logs)
    ;; Note that the double ampersand is a cross platform method to run 2 commands in 1 line
    (async-shell-command (concat "cd " tomcat-path " && " tomcat-catalina-name " run") "*tomcat*"))
  (defun tomcat-stop ()
    "Stop the tomcat process started by tomcat-start."
    (interactive)
    (interrupt-process (get-buffer-process "*tomcat*")))
  (defun tomcat-kill ()
    "Kill the tomcat process started by tomcat-start."
    (interactive)
    (kill-process (get-buffer-process "*tomcat*"))))

;;; LISP
;; Emacs Lisp
(use-package dash
  :ensure t
  :demand t
  :config
  (global-dash-fontify-mode)
  (with-eval-after-load 'info-look
    (dash-register-info-lookup)))
;; Clojure
(use-package clojure-ts-mode
  :ensure t)
(use-package cider
  :ensure t
  :hook ((clojure-ts-mode . rainbow-delimiters-mode)
         (clojure-ts-mode . lsp))
  :bind (("C-c b s" . clj-biff-start)
         ("C-c b x" . clj-biff-stop)
         ("C-c b c" . clj-biff-clear-logs))
  :custom
  (cider-clojure-cli-parameters "-A:dev"))
; Convert html to hiccup (hiccup-cli is better but requires an external program)
(if (executable-find "hiccup-cli")
    (use-package hiccup-cli
      :ensure t
      :bind (("C-c h h" . hiccup-cli-region-as-hiccup)
             ("C-c h y" . hiccup-cli-yank-as-hiccup)
             ("C-c h p" . hiccup-cli-paste-as-hiccup)))
  (use-package html-to-hiccup
    :ensure t
    :bind (("C-c h h" . html-to-hiccup-convert-region)
           ("C-c h y" . html-to-hiccup-yank))))
(use-package cider-hydra
  :ensure t
  :hook (clojure-mode . cider-hydra-mode))
;; Common Lisp
(use-package sly
  :ensure t
  :hook ((sly-mode . rainbow-delimiters-mode)
         (lisp-mode . rainbow-delimiters-mode))
  :config
  ;; Roswell has its own special configuration
  ;; After installing roswell run:
  ;; ros install sly
  (if (and (executable-find "ros")
           (file-exists-p (expand-file-name "~/.roswell/helper.el")))
      (load (expand-file-name "~/.roswell/helper.el"))
    (setq inferior-lisp-program "sbcl")))
;;; End LISP

;; CSV editing mode
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

;; Markdown
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'". gfm-mode)
         ("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  :config
  (setq markdown-open-command 'find-file))

;; C and C++
(use-package clang-format
  :ensure t)
(use-package cmake-mode
  :ensure t)

;; Python
(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize)))
;; Allows use of virtual environments in python
(use-package pet
  :ensure t
  :config
  (defun my-python-use-venv ()
    "Set up python to use venv. Run before opening project."
    (interactive)
    (add-hook 'python-base-mode-hook 'pet-mode -10)))

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

;; SQL
(use-package ejc-sql
  :ensure t
  :if (and (executable-find "clj")
           (executable-find "lein"))
  :hook ((ejc-sql-connected . (lambda ()
                                (ejc-set-fetch-size 99)
                                (ejc-set-max-rows 99)
                                (ejc-set-show-too-many-rows-message t)
                                (ejc-set-column-width-limit 25)
                                (ejc-set-use-unicode t)))
         (ejc-sql-mode . (lambda () (ejc-eldoc-setup))))
  :bind (:map ejc-sql-mode-keymap
              ("C-<return>" . ejc-eval-user-sql-at-point))
  :config
  (setq clomacs-httpd-default-port 9095
        ejc-sql-separator ";"
        ejc-completion-system 'standard
        ejc-result-table-impl 'orgtbl-mode))
;; This is only here to facilitate capfs for ejc-sql
(use-package company
  :ensure t)
(use-package ejc-company
  :ensure nil
  :after (ejc-sql company cape)
  :hook (ejc-sql-mode . (lambda () (setq-local completion-at-point-functions (mapcar #'cape-company-to-capf (list #'ejc-company-backend #'company-keywords))))))

(use-package sql
  :hook (sql-mode . sql-highlight-mariadb-keywords))
;;(sql-mode . ejc-sql-mode)
;(setq sql-mysql-options '("--prompt=mysql> " "-C" "-t" "-f" "-n"))

;; Web
(use-package web-mode
  :ensure t
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . html-mode) ; web mode crashes on some jsp files.
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html?\\'" . html-mode)))
(use-package js2-mode
  :ensure t
  :hook (js-mode . js2-minor-mode)
  ;; :mode (("\\.tsx\\'" . js-mode)
  ;;        ("\\.ts\\'" . js-mode))
  :config
  (if (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
      (setq inhibit-compacting-font-cache t)))
(use-package lsp-tailwindcss
  :ensure (:host github :repo "merrickluo/lsp-tailwindcss")
  :after lsp-mode
  :init
  (setq lsp-tailwindcss-add-on-mode t
        lsp-tailwindcss-server-version "0.14.8"
        lsp-tailwindcss-skip-config-check t))
(use-package emmet-mode
  :ensure t
  :hook (sgml-mode . emmet-mode)
  :bind (("C-<tab>" . emmet-expand-line)))

;; YAML
; Basic syntax highlighting in yaml
(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" . yaml-mode))
; Adds some additional functionality to yaml-mode
(use-package yaml-pro
  :ensure t
  :hook (yaml-mode . yaml-pro-mode))

;;; Misc
;; Rainbow parentheses highlighting
(use-package rainbow-delimiters
  :ensure t
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))
;; Highlight color codes
(use-package rainbow-mode
  :ensure t
  :hook (prog-mode . rainbow-mode))
;; Tree sitter
;;; The background on the difference between these plugins is that treesit-auto uses the built in treesit package, while tree-sitter-langs uses the tree-sitter package which predates the emacs treesit builtin.
;;; Preserving old configuration from when I was not using treesit for everything just in case.
;; (if (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
;;     (progn (use-package tree-sitter-langs
;;              :ensure t
;;              :demand t
;;              :after tree-sitter
;;              :hook (tree-sitter-after-on . tree-sitter-hl-mode)
;;              :config
;;              (global-tree-sitter-mode))
;;            (use-package tree-sitter
;;              :ensure t
;;              :demand t
;;              :init
;;              (setq treesit-font-lock-level 4)))
;;   (progn (use-package treesit-auto
;;            :ensure t
;;            :custom
;;            (treesit-auto-install nil)
;;            (treesit-extra-load-path (list (expand-file-name "tree-sitter/linux" user-emacs-directory)
;;                                           (expand-file-name "tree-sitter/macos" user-emacs-directory)
;;                                           (expand-file-name "tree-sitter/win" user-emacs-directory)))
;;            :config
;;            (treesit-auto-add-to-auto-mode-alist 'all)
;;            (global-treesit-auto-mode))
;;          (use-package treesit
;;            :ensure nil
;;            :demand t
;;            :custom
;;            (treesit-font-lock-level 4))))
(use-package treesit
  :ensure nil
  :demand t
  :custom
  (treesit-font-lock-level 4)
  (treesit-extra-load-path (list (expand-file-name "tree-sitter/linux" user-emacs-directory)
                                 (expand-file-name "tree-sitter/macos" user-emacs-directory)
                                 (expand-file-name "tree-sitter/win" user-emacs-directory))))
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install nil)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))
;; Show current function in modeline.
(use-package which-func
  :ensure nil
  :demand t
  :config
  (setq which-func-modes '(java-mode
                           org-mode
                           emacs-lisp-mode
                           lisp-mode
                           clojure-mode
                           lsp-mode
                           xml-mode
                           nxml-mode
                           python-mode
                           js-ts-mode))
  (which-function-mode))
;; Stick the function name of the top function to the top of the screen.
(use-package semantic
  :ensure nil
  ;; Only use this when not using lsp.
  :bind ("C-c b r" . semantic-stickyfunc-mode)
  :config
  (semantic-mode)
  (global-semanticdb-minor-mode))
;;; lang.el ends here
