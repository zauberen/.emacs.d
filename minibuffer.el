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
         ("C-'" . consult-bookmark)
         ("C-c m" . bookmark-set)
         ("C-c b d" . bookmark-delete)
         ("C-c b m" . bookmark-set)
         ("C-c b l" . consult-bookmark)
         ("C-x r l" . consult-bookmark) ; Replace the existing bookmark list with consult, not that I'd use it
         ("C-c h" . consult-history)
         ("C-," . consult-yank-from-kill-ring)
         ("C-l" . consult-line-multi)
         ("C-x b" . consult-buffer))
  :config
  ;; Add a slight waiting period before a preview
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult-projectile
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.5 any))
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
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind (("C-." . vertico-repeat)
         :map vertico-map
              ("C-q" . vertico-quick-insert)
              ("M-q" . vertico-quick-exit))
  :config
  (vertico-mode)
  (evil-define-key 'normal 'global
    (kbd "C-.") #'vertico-repeat))
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
