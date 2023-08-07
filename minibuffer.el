;;; minibuffer.el --- Minibuffer plugins
;;; Commentary:
;;; consult, consult-flycheck, vertico, marginalia, embark, embark-consult
;;; Code:
(use-package consult
  :ensure t
  :after evil
  :demand t
  :config
  (setq completion-in-region-function #'consult-completion-in-region)
  (evil-define-key 'normal 'global
    (kbd "SPC j") #'evil-collection-consult-jump-list))
(use-package consult-flycheck
  :ensure t
  :demand t
  :after consult flycheck)
(use-package vertico
  :ensure t
  :demand t
  :config
  (vertico-mode))
(use-package marginalia
  :ensure t
  :demand t
  :config
  (marginalia-mode))
(use-package embark
  :ensure t
  :demand t
  :after evil
  ; Bind both C-c e (better pneumonics) and C-c v (matches evil better)
  :bind (("C-c e" . embark-act)
         ("C-c v" . embark-act)
         ("C-;" . embark-act))
  :config
  (evil-define-key 'normal 'global
    (kbd "SPC v") #'embark-act))
(use-package embark-consult
  :ensure t
  :demand t
  :after embark consult)
;;; minibuffer.el ends here
