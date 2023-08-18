;;; snippets.el --- Snippet plugin configuration
;;; Commentary:
;;; yasnippet-snippets, yasnippet, tempel, tempel-collection
;;; Code:
(use-package yasnippet-snippets
  :ensure t
  :pin melpa)
(use-package yasnippet
  :ensure t
  :pin melpa
  :demand t
  :after yasnippet-snippets
  :diminish yas-minor-mode
  :bind ("C-c y" . yas-insert-snippet)
  :hook ((prog-mode . tempel-abbrev-mode))
  :config
  (evil-define-key 'normal 'global
    (kbd "SPC y") #'yas-insert-snippet)
  (yas-global-mode 1))
(use-package tempel
  :ensure t
  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert)
         :map tempel-map
         ("M-n" . tempel-next)
         ("M-p" . tempel-previous))
  :init
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
                      completion-at-point-functions)))
  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  (global-tempel-abbrev-mode))
(use-package tempel-collection
  :ensure t)
;;; snippets.el ends here
