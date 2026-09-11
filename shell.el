;;; shell.el --- Universal eshell and shell configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package eshell
  :bind ("C-x e" . eshell)
  :hook (eshell-mode . (lambda ()
                         (eshell/alias "open" "find-file $1")
                         (eshell/alias "emacs" "find-file $1")
                         (eshell/alias "vim" "find-file $1")
                         (eshell/alias "vi" "find-file $1")
                         (eshell/alias "less" "find-file $1")
                         (eshell/alias "e" "find-file $1")
                         ; These open files in a separate window
                         (eshell/alias "ee" "find-file-other-window $1")
                         ; Open folder in explorer on windows
                         (when (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
                             (eshell/alias "ex" "explorer .")))))
(when (not (or (eq system-type 'ms-dos) (eq system-type 'windows-nt)))
  (use-package eat
    :ensure t
    :demand t
    :config
    (eat-eshell-mode)
    (eat-eshell-visual-command-mode)))

;; Ghostty based shell, usable in all OS'
(use-package ghostel
  :ensure t
  :bind (("C-x m" . ghostel)))
(use-package evil-ghostel
  :ensure t
  :after (ghostel evil)
  :hook (ghostel-mode . evil-ghostel-mode))
;;; shell.el ends here
