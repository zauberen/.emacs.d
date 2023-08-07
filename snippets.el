;;; snippets.el --- Yasnippet configuration
;;; Commentary:
;;; yasnippet-snippets, yasnippet
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
  :config
  (evil-define-key 'normal 'global
    (kbd "SPC y") #'yas-insert-snippet)
  (yas-global-mode 1))
;;; snippets.el ends here
