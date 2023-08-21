;;; hydra.el --- Hydra configuration
;;; Commentary:
;;; hydra
;;; Code:
(use-package hydra
  :ensure t)

(use-package ace-window
  :ensure t
  :bind ("C-c C-w" . ace-window)
  :init
  (evil-define-key 'normal 'global
    (kbd "SPC w") #'ace-window)
  (setq aw-dispatch-always t
        aw-keys '(?h ?j ?k ?l ?a ?s ?d ?f ?g)
        aw-dispatch-alist '((?q aw-delete-window "Delete")
                            (?m aw-move-window "Move")
                            (?y aw-copy-window "Copy")
                            (?n aw-flip-window "Previous")
                            (?w aw-swap-window "Swap")
                            (?o delete-other-windows "Delete other windows")
                            (?? aw-show-dispatch-help))))
;;; hydra.el ends here
