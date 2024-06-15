;;; hydra.el --- Hydra configuration
;;; Commentary:
;;; hydra
;;; Code:
;; Hydra makes pretty menus for complex commands
(use-package hydra
  :ensure t)

;; Switch windows using ace style bindings
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

;; Useful dictionary/thesaurus program (requires internet)
;(use-package powerthesaurus
  ;:after hydra evil
  ;:ensure t
  ;:config
  ;(evil-define-key 'normal 'global
    ;(kbd "SPC d") #'powerthesaurus-hydra/body)
  ;(evil-define-key 'visual 'global
    ;(kbd "SPC d") #'powerthesaurus-hydra/body))
;;; hydra.el ends here
