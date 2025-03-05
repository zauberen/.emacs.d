;;; tweaks.el --- Editor tweaks -*- lexical-binding: t -*-
;;; Commentary:
;;; No packages.
;;; Code:
;; Hide welcome screen
(setq inhibit-startup-screen t
      initial-scratch-message nil
      server-client-instructions nil)
;; Buffer settings
(setq use-short-answers t
      confirm-kill-processes nil
      kill-buffer-query-functions nil
      auth-source-save-behavior nil
      enable-local-variables :safe
      disabled-command-function nil)

;; Increase undo history
(setq undo-limit (* 4 1024 1024)
      undo-strong-limit (* 6 1024 1024)
      kill-ring-max 512
      kill-do-not-save-duplicates t)
;; Update files modified on disk
(setq global-auto-revert-non-file-buffers t)
; Fixes some auto save issues with encoding
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
;; Formatting
(setq-default fill-column 80
              indent-tabs-mode nil
              tab-width 4
              tab-always-indent 'complete
              require-final-newline t)
;(setq indent-line-function 'insert-tab)
(setq sentence-end-double-space nil)
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

;; Fix blurry PDFs
(use-package doc-view
  :init
  ;; Set DPI of pdf viewer
  (setq doc-view-resolution 300
        ;; Set viewer to use scaling? (from reddit)
        pdf-view-use-scaling t))
;;; tweaks.el ends here
