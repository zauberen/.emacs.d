;;; git.el --- Git related packages
;;; Commentary:
;;; magit, magit-todos, git-gutter
;;; Code:
;; Git config
;; Transient required because the magit recipe does not pull the correct version
(use-package transient
  :ensure (:host github :repo "magit/transient"))
(use-package magit
  :ensure (:host github :repo "magit/magit")
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
(use-package git-gutter
  :ensure t
  :demand t
  :config
  (custom-set-variables '(git-gutter:update-interval 2)))
; Old config for git gutter
; Need to test the existing config on a terminal
  ;(custom-set-variables '(git-gutter:modified-sign " ")
                        ;'(git-gutter:added-sign " ")
                        ;'(git-gutter:deleted-sign " "))
  ;(set-face-background 'git-gutter:modified "purple")
  ;(set-face-background 'git-gutter:added "green")
  ;(set-face-background 'git-gutter:deleted "red")
(use-package git-gutter-fringe
  :ensure t
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom)
  (global-git-gutter-mode t))
;;; git.el ends here
