;;; web.el --- Web language packages
;;; Commentary:
;;; html-mode, js2-mode
;;; Code:
(use-package html-mode
  :mode (("\\.html$" . html-mode)
         ("\\.html\\'" . html-mode)))
(use-package js2-mode
  :ensure t
  :pin melpa
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx?\\'" . js2-minor-mode))
  :init
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode)))
;;; web.el ends here
