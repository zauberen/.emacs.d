;;; minibuffer.el --- Minibuffer plugins -*- lexical-binding: t -*-
;;; Commentary:
;;; consult, consult-flycheck, vertico, marginalia, embark, embark-consult
;;; Code:
(use-package consult
  :ensure t
  :after evil evil-collection
  :demand t
  :bind (("C-c o a" . consult-org-agenda)
         ("C-'" . consult-bookmark)
         ("C-c m" . bookmark-set)
         ("C-c b d" . bookmark-delete)
         ("C-c b m" . bookmark-set)
         ("C-c b l" . consult-bookmark)
         ("C-x r l" . consult-bookmark) ; Replace the existing bookmark list with consult, not that I'd use it
         ;("C-c h" . consult-history)
         ("C-c s" . consult-line)
         ("C-c C-s" . consult-line-multi)
         ("C-c t o" . consult-outline)
         ("C-," . consult-yank-from-kill-ring)
         ("C-x b" . consult-buffer))
  :config
  ;; Bind C-s to ripgrep if it is installed. Git is required, so git-grep will always be there as a fallback.
  (if (not (eq (executable-find "rg") nil))
      (define-key global-map (kbd "C-s") 'consult-ripgrep)
    (define-key global-map (kbd "C-s") 'consult-git-grep))
  ;; Add a slight waiting period before a preview on windows
  (when (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
    (consult-customize
     consult-ripgrep consult-git-grep consult-grep
     consult-bookmark consult-recent-file consult-xref
     consult-projectile consult-buffer consult-notes
     consult--source-bookmark consult--source-file-register
     consult--source-recent-file consult--source-project-recent-file
     :preview-key '(:debounce 0.5 any)))
  ; Set minibuffer completion default to consult
  (setq completion-in-region-function #'consult-completion-in-region
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  ; Adds recently opened files to consult-buffer
  (recentf-mode)
  ; Use consult-line with evil
  (defun consult-line-evil-history (&rest _)
    "Add latest `consult-line' search pattern to the evil search history ring.
This only works with orderless and for the first component of the search."
    (when (and (bound-and-true-p evil-mode)
               (eq evil-search-module 'evil-search))
      (let ((pattern (car (orderless-pattern-compiler (car consult--line-history)))))
        (add-to-history 'evil-ex-search-history pattern)
        (setq evil-ex-search-pattern (list pattern t t))
        (setq evil-ex-search-direction 'forward)
        (when evil-ex-search-persistent-highlight
          (evil-ex-search-activate-highlight evil-ex-search-pattern)))))

  (advice-add #'consult-line :after #'consult-line-evil-history)
  ; Use consult for xref
  (evil-define-key 'normal 'global
    ; Bind to SPC o because evil-jump-backward is C-o
    (kbd "SPC o") #'evil-collection-consult-jump-list
    (kbd "C-t") #'consult-imenu
    (kbd "C-S-t") #'consult-imenu-multi
    (kbd "SPC b") #'consult-buffer
    (kbd "g m") #'evil-collection-consult-mark))
(use-package consult-projectile
  :ensure t
  :demand t
  :after projectile evil
  :config
  ; Rebind SPC f to use consult
  (evil-define-key 'normal 'global
    (kbd "SPC f") #'consult-projectile
    (kbd "SPC p") #'consult-projectile-switch-project))
(use-package consult-flycheck
  :ensure t
  :after consult flycheck
  :demand t
  :bind ("C-c e" . consult-flycheck))
(use-package vertico
  :ensure t
  :demand t
  :hook ((minibuffer-setup . vertico-repeat-save)
         (rfn-eshadow-update-overlay . vertico-directory-tidy))
  :bind (("C-." . vertico-repeat)
         :map vertico-map
         ("C-q" . my-vertico-avy-autofill)
         ("M-q" . my-vertico-avy-run)
         ("DEL" . vertico-directory-delete-char))
  :config
  (defun my-vertico-avy-autofill ()
    "Autofill jump in vertico."
    (interactive)
    (vertico-multiform-grid)
    (vertico-quick-insert))
  (defun my-vertico-avy-run ()
    "Jump and run in vertico."
    (interactive)
    (vertico-multiform-grid)
    (vertico-quick-exit))
  (evil-define-key 'normal 'global
    (kbd "C-.") #'vertico-repeat)
  (setq enable-recursive-minibuffers t)
  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t)
  (vertico-mode))
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; Use backspace to delete a directory
  :bind (:map vertico-map
         ("DEL" . vertico-directory-delete-char))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))
(use-package marginalia
  :ensure t
  :after vertico
  :custom
  ;; Set this to a ridiculously large number so that it always calculates the field width dynamically
  (marginalia-field-width 500)
  :config
  (marginalia-mode))
(use-package embark
  :ensure t
  :after evil
  :bind (("C-;" . embark-act)
         ("C-:" . embark-collect)))
(use-package embark-consult
  :ensure t
  :after embark consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))
;;; minibuffer.el ends here
