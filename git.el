;;; git.el --- Git related packages
;;; Commentary:
;;; magit, magit-todos, git-gutter
;;; Code:
;; Git config
(use-package magit-todos
  :ensure t)
(use-package magit
  :ensure t
  :pin melpa
  :after magit-todos
  :init
  (setq magit-view-git-manual-method 'man
        transient-history-file null-device
        magit-save-repository-buffers 'dontask
        magit-delete-by-moving-to-trash nil)
  :config
  ;; TODO convert this to use-package
  (with-eval-after-load 'magit
    (remove-hook 'server-switch-hook #'magit-commit-diff)
    (magit-todos-mode)))
(use-package git-gutter
  :ensure t
  :pin melpa
  :demand t
  :config
  (custom-set-variables '(git-gutter:update-interval 2))
  (custom-set-variables '(git-gutter:modified-sign " ")
                        '(git-gutter:added-sign " ")
                        '(git-gutter:deleted-sign " "))
  (set-face-background 'git-gutter:modified "purple")
  (set-face-background 'git-gutter:added "green")
  (set-face-background 'git-gutter:deleted "red")
  (global-git-gutter-mode t))
;;; git.el ends here
