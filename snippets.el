;;; snippets.el --- Snippet plugin configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Snippet packages
;;; Code:
(use-package yasnippet-snippets
  :ensure t)
(use-package common-lisp-snippets
  :ensure t)
(use-package yasnippet
  :ensure t
  :after (yasnippet-snippets evil)
  :diminish yas-minor-mode
  :bind ("C-c y" . yas-insert-snippet)
  :config
  (evil-define-key 'normal 'global
    (kbd "SPC y") #'yas-insert-snippet)
  (yas-global-mode 1))
(use-package yasnippet-capf
  :ensure (:host github :repo "zauberen/yasnippet-capf")
  :after cape yasnippet
  :config
  (setq yasnippet-capf-lookup-by 'name))
(use-package tempel
  :ensure t
  :demand t
  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert)
         :map tempel-map
         ("M-n" . tempel-next)
         ("M-p" . tempel-previous))
  :config
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-complete
                      completion-at-point-functions))))
  ;(add-hook 'conf-mode-hook 'tempel-setup-capf)
  ;(add-hook 'prog-mode-hook 'tempel-setup-capf)
  ;(add-hook 'text-mode-hook 'tempel-setup-capf))
  ;; Make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; This makes it so if you type an abbrev and any character after,
  ;; it will print the abbreviation, so in java typing if dumps the if abbrev
  ;; that was too annoying so I turned it off
  ;(global-tempel-abbrev-mode))
(use-package tempel-collection
  :ensure t)
;;; snippets.el ends here
