;;; lang.el --- Misc language configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Programming language configuration.
;;; Code:
(use-package compile
  :ensure nil
  :custom
  (compilation-scroll-output t))

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
  :mode (("\\.inc\\'" . java-ts-mode))
  :hook (java-ts-mode . (lambda ()
                          (when (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
                            (citre-use-global-windows))))
  :bind (("C-c t s" . tomcat-start)
         ("C-c t x" . tomcat-stop)
         ("C-c t c" . tomcat-clear-logs))
  :init
  (if (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
      (setq lsp-java-java-path "C:/Program Files/Eclipse Adoptium/jdk-21.0.9.10-hotspot/bin/java.exe"
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
(use-package clojure-mode
  :ensure t)
(use-package cider
  :ensure t
  :hook ((clojure-mode . rainbow-delimiters-mode)
         (clojure-mode . lsp))
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

;; perl
(use-package cperl-mode
  :ensure nil
  :hook (cperl-mode . flycheck-mode)
  :custom
  (cperl-indent-parens-as-block t)
  (cperl-close-paren-offset (- cperl-indent-level))
  :init
  (fset 'perl-mode 'cperl-mode))

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
                                (ejc-set-column-width-limit 50)
                                (ejc-set-use-unicode t)))
         (ejc-sql-mode . (lambda () (ejc-eldoc-setup))))
  :bind (:map ejc-sql-mode-keymap
              ("C-<return>" . ejc-eval-user-sql-at-point))
  :init
  (defun ejc-mariadb (display-name user password ip &optional port defaultdb)
    "Add an ejc connection for a MariaDB database which displays DISPLAY-NAME when selecting the db in ejc-connect."
    (ejc-create-connection
     display-name
     :dependencies [[org.mariadb.jdbc/mariadb-java-client "3.5.3"]]
     :classname "org.mariadb.jdbc.Driver"
     :connection-uri (concat "jdbc:mariadb://"
                             ip
                             (if (eq port nil)
                                 ""
                               (concat ":" port))
                             (if (eq defaultdb nil)
                                 ""
                               (concat "/" defaultdb)))
     :user user
     :password password))
  (defun ejc-postgres (display-name user password ip &optional port defaultdb)
    "Add an ejc connection for a PostgreSQL database which displays DISPLAY-NAME when selecting the db in ejc-connect."
    (ejc-create-connection
     display-name
     :dependencies [[org.postgresql/postgresql "42.7.8"]]
     :classname "org.postgresql.Driver"
     :connection-uri (concat "jdbc:postgresql://"
                             ip
                             (if (eq port nil)
                                 ""
                               (concat ":" port))
                             (if (eq defaultdb nil)
                                 ""
                               (concat "/" defaultdb)))
     :user user
     :password password))
  (defun ejc-oracle (display-name user password ip &optional port sid)
    "Add an ejc connection for an Oracle database which displays DISPLAY-NAME when selecting the db in ejc-connect."
    (ejc-create-connection
     display-name
     :dependencies [[com.oracle.database.jdbc/ojdbc8 "23.9.0.25.07"]]
     :classname "oracle.jdbc.OracleDriver"
     :connection-uri (concat "jdbc:oracle:thin:@"
                             ip
                             (if (eq port nil)
                                 ""
                               (concat ":" port))
                             (if (eq sid nil)
                                 ""
                               (concat ":" sid)))
     :user user
     :password password
     :separator "/"))
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
(use-package npm
  :ensure t
  :init
  (defun npm-run-local ()
    "Run local npm script."
    (interactive)
    (npm-common--compile (npm-run--get-run-command "local")))
  :bind (("C-c n p" . npm)
         ("M-R" . npm-run-local)))

(use-package elisp-mode
  :ensure nil
  :init
  (defun my-elisp-lookup-evil ()
    "Look up the value of the function or variable at point"
    (let ((var-at-point (variable-at-point)))
      (if (eq var-at-point 0)
          (describe-function (function-called-at-point))
        (describe-variable var-at-point))))
  :hook (emacs-lisp-mode . (lambda () (setq-local evil-lookup-func 'my-elisp-lookup-evil))))

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
;; PHP
(use-package php-mode
  :ensure t
  :hook (php-mode . php-ts-mode))
; PHP bindings to elisp
(use-package php-runtime
  :ensure t)
; Run unit tests
(use-package phpunit
  :ensure t)
; REPL for php, wget psysh.org/psysh;chmod +x psysh;sudo cp psysh /usr/bin
(use-package psysh
  :ensure t)
; Error checking
(use-package phpstan
  :ensure t)
(use-package flycheck-phpstan
  :ensure t)
; Package management in PHP
(use-package composer
  :ensure t)

;; ASM
(use-package nasm-mode
  :ensure (:host github :repo "8dcc/nasm-mode" :branch "require-nasmtok"))

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
  (treesit-font-lock-level 4))
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
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

;; Local doc viewer
(use-package devdocs
  :ensure t
  :bind (("C-c s s" . devdocs-lookup)
         ("C-c s p" . devdocs-peruse)
         ("C-c s i" . devdocs-install)
         ("C-c s d" . devdocs-delete)
         ("C-c s u" . devdocs-update-all))
  :hook ((java-ts-mode . (lambda () (my-devdocs-lang-hook "openjdk~8" "openjdk~8_web" "openjdk~8_gui")))
         (js-ts-mode . (lambda () (my-devdocs-lang-hook "javascript" "dom")))
         (css-ts-mode . (lambda () (my-devdocs-lang-hook "css")))
         (bash-ts-mode . (lambda () (my-devdocs-lang-hook "bash")))
         (clojure-mode . (lambda () (my-devdocs-lang-hook "clojure~1.11")))
         (emacs-lisp-mode . (lambda () (my-devdocs-lang-hook "elisp"))))
  :config
  (setq evil-lookup-func #'devdocs-lookup)
  :init
  (defun my-install-docs (&rest docs)
    "Install a set of devdocs."
    (dolist (doc docs)
      (devdocs-install doc)))
  (defun my-devdocs-lang-hook (&rest docs)
    "Set DOCS as the docs for the local buffer."
    (setq-local devdocs-current-docs docs))
  (defun my-install-my-docs ()
    "Install all of the devdocs I currently use."
    (interactive)
    (my-install-docs "bash" "css" "dom" "elisp" "javascript"
                     "openjdk~8" "openjdk~8_web" "openjdk~8_gui"
                     "git" "html" "http" "mariadb"
                     "clojure~1.11" "perl~5.38")))
;;; lang.el ends here
