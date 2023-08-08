;;; minibuffer.el --- Minibuffer plugins
;;; Commentary:
;;; consult, consult-flycheck, vertico, marginalia, embark, embark-consult
;;; Code:
(use-package consult
  :ensure t
  :pin melpa
  :after evil evil-collection
  :demand t
  :bind (("C-s" . consult-git-grep)
         ("C-c s" . consult-org-agenda)
         ("C-c b" . consult-bookmark)
         ("C-c h" . consult-history)
         ("C-," . consult-yank-from-kill-ring)
         ("C-l" . consult-line-multi)
         ("C-x b" . consult-buffer))
  :config
  ; Set minibuffer completion default to consult
  (setq completion-in-region-function #'consult-completion-in-region
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  ; Adds recently opened files to consult-buffer
  (recentf-mode)
  ; Use consult for xref
  (evil-define-key 'normal 'global
    (kbd "SPC j") #'evil-collection-consult-jump-list
    (kbd "SPC b") #'consult-buffer
    (kbd "/") #'consult-line
    (kbd "g m") #'evil-collection-consult-mark))
(use-package consult-projectile
  :ensure t
  :after projectile evil
  :demand t
  :config
  ; Rebind SPC f to use consult
  (evil-define-key 'normal 'global
    (kbd "SPC f") #'consult-projectile))
(use-package consult-flycheck
  :ensure t
  :after consult flycheck
  :demand t
  :bind ("C-c e" . consult-flycheck))
(use-package vertico
  :ensure t
  :demand t
  :config
  (vertico-mode))
(use-package marginalia
  :ensure t
  :pin melpa
  :demand t
  :config
  (marginalia-mode))
(use-package embark
  :ensure t
  :pin melpa
  :demand t
  :after evil
  :bind ("C-;" . embark-act))
(use-package embark-consult
  :ensure t
  :pin melpa
  :demand t
  :after embark consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))
;;; minibuffer.el ends here
