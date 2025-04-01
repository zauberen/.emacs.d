;;; git.el --- Git related packages -*- lexical-binding: t -*-
;;; Commentary:
;;; magit, magit-todos, git-gutter
;;; Code:
;; Git config
;; Transient required because the magit recipe does not pull the correct version
(use-package transient
  :ensure (:host github :repo "magit/transient"))
(use-package magit
  :ensure (:host github :repo "magit/magit")
  :bind ("C-x v f" . magit-log-buffer-file)
  :after transient
  :init
  (setq magit-view-git-manual-method 'man
        transient-history-file null-device
        magit-save-repository-buffers 'dontask
        magit-delete-by-moving-to-trash nil
        magit-define-global-key-bindings 'recommended
        magit-blame-styles '((msg-only (show-message . t))
                             (margin
                              (margin-format " %s%f" " %C %a")
                              (margin-width . 42)
                              (margin-face . magit-blame-margin)
                              (margin-body-face magit-blame-dimmed)))
        magit-blame-echo-style 'msg-only))
(use-package vc-msg
  :ensure t
  :bind ("C-x v p" . #'vc-msg-show))

;; Moving to diff-hl for now
;; (use-package git-gutter
;;   :ensure t
;;   :demand t
;;   :config
;;   (custom-set-variables '(git-gutter:update-interval 2)))
;; (use-package git-gutter-fringe
;;   :ensure t
;;   :config
;;   (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
;;   (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
;;   (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom)
;;   (global-git-gutter-mode t))

(use-package diff-hl
  :ensure t
  :demand t
  :init
  (setq diff-hl-disable-on-remote t)
  :config
  (global-diff-hl-mode)
  ;; Normal diff-hl doesn't work on terminal, need to use the less-good hl-margin-mode
  (unless (display-graphic-p)
    (diff-hl-margin-mode)))
(use-package diff-hl-dired
  :after diff-hl
  :hook (dired-mode . diff-hl-dired-mode-unless-remote))
;;; git.el ends here
