;;; web.el --- Web language packages
;;; Commentary:
;;; html-mode, js2-mode
;;; Code:
(use-package html-mode
  :mode (("\\.html$" . html-mode)
         ("\\.html\\'" . html-mode)))
(use-package js2-mode
  :ensure t
  :hook (js-mode . js2-minor-mode))
;;; web.el ends here
