;;; eshell.el --- Universal eshell configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package eshell
  :bind ("C-x e" . eshell)
  :hook (eshell-mode . (lambda ()
                         ; Quick switch to org
                         (eshell/alias "so"
                                       (if (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
                                           "cd C:/org/org-notes"
                                           "cd ~/Documents/GitHub/org-notes"))
                         ; Opening files, these open in-frame
                         (eshell/alias "e" "find-file $1")
                         (eshell/alias "ff" "find-file $1")
                         (eshell/alias "emacs" "find-file $1")
                         ; These open files in a separate frame
                         (eshell/alias "ee" "find-file-other-window $1")
                         ; Just in case an oopsie occurs
                         (eshell/alias "vim" "find-file-other-window $1")
                         (eshell/alias "vi" "find-file-other-window $1")
                         (eshell/alias "less" "find-file-other-window $1")
                         ; Open folder in explorer on windows
                         (when (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
                             (eshell/alias "ex" "explorer ."))
                         ; Git
                         (eshell/alias "gd" "magit-diff-unstaged")
                         (eshell/alias "gds" "magit-diff-staged")
                         ; Dired
                         (eshell/alias "d" "dired .")
                         ; The 'ls' executable requires the Gnu version on the Mac
                         (let ((ls (if (file-exists-p "/usr/local/bin/gls")
                                       "/usr/local/bin/gls"
                                       "ls")))
                           ; This is here because it uses the ls variable set above
                           (eshell/alias "ll" (concat ls " -AlohG --color=always"))))))

(use-package eat
  :ensure t
  :demand t
  :config
  (when (not (or (eq system-type 'ms-dos) (eq system-type 'windows-nt)))
    (eat-eshell-mode)
    (eat-eshell-visual-command-mode)))
;;; eshell.el ends here
