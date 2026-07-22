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
         (dap-stopped . (lambda (&_rest) (call-interactively #'dap-hydra)))
         (dap-terminated . (lambda (&_rest) (dap-hydra/nil))))
  :bind (:map lsp-mode-map
              ("M-S-d" . dap-debug)
              ("M-d" . dap-hydra)
              ("C-c b b" . dap-java-run-last-test)
              ("C-c b r" . dap-java-run-test-method))
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
         ("C-c t c" . tomcat-clear-logs)
         ("C-c m r" . maven-run))
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
  ;; Modified to not use environment variables
  (defun dap-java--run-unit-test-command (runner dwim?)
    "Run debug test with the following arguments.
RUNNER is the test executor.  DWIM? when t it will try to run the
surrounding method.  Otherwise it will run the surrounding test."
    (-let* ((run-method? (and dwim? (dap-java-test-method-at-point t)))
            (to-run (if run-method?
                        (dap-java-test-method-at-point)
                      (dap-java-test-class)))
            (test-class-name (cl-first (s-split "#" to-run)))
            (class-path (->> (with-lsp-workspace (lsp-find-workspace 'jdtls)
                               (lsp-send-execute-command "vscode.java.resolveClasspath"
                                                         (vector test-class-name nil)))
                             cl-second
                             (s-join dap-java--classpath-separator)))
            (prog-list (if dap-java-use-testng
                           (cl-list* runner
                                     "-cp" class-path
                                     "org.testng.TestNG"
                                     "-d" dap-java-testng-report-dir
                                     (if (and (s-contains? "#" to-run) run-method?) "-methods" "-testclass")
                                     (if run-method? (s-replace "#" "." to-run) test-class-name)
                                     dap-java-test-additional-args)
                         (cl-list* runner "-jar" dap-java-test-runner
                                   "-cp" class-path
                                   (if (and (s-contains? "#" to-run) run-method?) "-m" "-c")
                                   (if run-method? to-run test-class-name)
                                   dap-java-test-additional-args))))
      (list :program-to-start (s-join " " prog-list)
            :environment-variables `(("JUNIT_CLASS_PATH" . ,class-path))
            :name to-run
            :cwd (lsp-java--get-root))))
  ;; current VSCode defaults
  (setq lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx10G" "-Xms1G")
        ;; Default path, change this in local.el!
        tomcat-path "~/tomcat"
        ;; Set the name of the catalina script. If using binary distributions, this should work out of the box.
        tomcat-catalina-name (if (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
                                 "catalina.bat"
                               "catalina.sh"))
  ;; Tomcat management (recommend running in a dedicated frame)
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
    (kill-process (get-buffer-process "*tomcat*")))
  ;; Maven control
  (defun maven-run ()
    "Run maven in the project root."
    (interactive)
    (tomcat-clear-logs)
    (let* ((projectile-folder (projectile-project-root))
           (folder (if (eq projectile-folder nil)
                       default-directory
                     projectile-folder))
           (mvn-buffer (get-buffer-create (concat "*Maven Run - " folder))))
      (with-current-buffer mvn-buffer (erase-buffer))
      (async-shell-command (concat "cd " folder " && mvn exec:java") (concat "*Maven Run - " folder "*")))))

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
  :hook ((clojure-mode . rainbow-delimiters-mode))
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
;; Janet
;; Installation:
;; - Install janet in some way..
;; - git clone https://github.com/janet-lang/jpm
;; - cd jpm; janet bootstrap.janet
;; - Done! A common lib I use:
;; - jpm install sh
(use-package janet-ts-mode
  :after evil
  :hook (janet-ts-mode . rainbow-delimiters-mode)
  :ensure (:host github
           :repo "sogaiu/janet-ts-mode"
           :files ("*.el")))
(use-package ajrepl
  :after janet-ts-mode
  :ensure (:host github
           :repo "sogaiu/ajrepl"
           :files ("*.el" "ajrepl"))
  :hook (janet-ts-mode . ajrepl-interaction-mode)
  :config
  ;; Better handling for evil
  (defun ajrepl-send-expression-at-point ()
    "Send expression at point."
    (interactive)
    (save-excursion
      (let ((end (+ (point) 1)))
        (save-excursion
          (skip-chars-backward " \t\n")
          (beginning-of-line)
          (when (looking-at "[ \t]*#")
            (setq end (point))))
        (when-let ((beg (ajrepl--column-zero-target-backward)))
          (when-let ((code (ajrepl--helper beg end)))
            (ajrepl-send-code code))))))
  (defun ajrepl-doc ()
    "Define janet symbol at point."
    (interactive)
    (ajrepl-send-code (concat "(doc " (thing-at-point 'symbol) ")")))
  (evil-define-key 'normal janet-ts-mode-map (kbd "K") 'ajrepl-doc))
(use-package flycheck-janet
  :ensure (:host github
           :repo "sogaiu/flycheck-janet"
           :files ("*.el")))
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
(use-package simple-httpd
  :ensure (:host github :repo "skeeto/emacs-web-server"))
(use-package ejc-sql
  :ensure (:host github :repo "zauberen/ejc-sql" :ref "blob-display-mariadb")
  :after simple-httpd
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
;; (use-package company
;;   :ensure t)
;; (use-package ejc-company
;;   :ensure nil
;;   :after (ejc-sql company cape)
;;   :custom
;;   (ejc-complete-on-dot t)
;;   :hook (ejc-sql-mode . (lambda () (setq-local completion-at-point-functions (mapcar #'cape-company-to-capf (list #'ejc-company-backend #'company-keywords))))))
(use-package ejc-capf
  :ensure nil
  :after ejc-sql
  :hook (ejc-sql-mode . (lambda () (setq-local completion-at-point-functions (list #'ejc-capf)))))

(use-package sql
  :hook (sql-mode . sql-highlight-mariadb-keywords))

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
  :hook (js-ts-mode . js2-minor-mode)
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

;; KDL (niri configuration)
(use-package kdl-mode
  :ensure t)

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
; Removed due to packaging issues.
;(use-package phpstan
;  :ensure (:host github :repo "emacs-php/phpstan.el"))
;(use-package flycheck-phpstan
;  :ensure t)
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
