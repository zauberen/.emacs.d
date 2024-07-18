;;; lang.el --- Misc language configuration
;;; Commentary:
;;; java-mode, csv-mode, pascal-mode, markdown-mode, clang-format, cmake-mode, rust-mode, cargo, sql
;;; Code:

;; Java Configuration
(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1)
  (dap-auto-configure-mode))
; Java for jsp files
(use-package java-mode
  :after citre
  ; Use global to get reference jumping on java projects, also no auto saves or git gutter to avoid encoding popups
  :hook (java-mode . (lambda ()
                       (when (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
                         (citre-use-global-windows)
                         (setq-local auto-save-default nil)
                         (git-gutter-mode nil)
                         (require 'dap-java))))
  :mode ("\\.jsp\\'" . java-mode))
(use-package lsp-java
  :ensure t
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
  (setq lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx2G" "-Xms100m")))

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
(use-package sql
  :hook ((sql-mode . sql-highlight-mariadb-keywords)
         (sql-interactive-mode . (lambda () (toggle-truncate-lines t))))
  :init
  (setq sql-mysql-options '("--prompt=mysql> " "-C" "-t" "-f" "-n")))

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
  :bind ("C-c r" . rainbow-delimiters-mode))

;;; lang.el ends here
