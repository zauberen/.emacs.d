;;; corfu.el --- Packages for completion at point
;;; Commentary:
;;; cape, corfu, corfu-terminal
;;; Code:
(use-package cape
  :ensure t
  :pin melpa
  :after citre
  :init
  (setq cape-dabbrev-min-length 2)
  :config
  (if (eq system-type 'darwin)
      (setq completion-at-point-functions (list #'cape-file
                                                (cape-super-capf #'cape-keyword
                                                                 #'cape-dabbrev
                                                                 #'citre-completion-at-point
                                                                 #'cape-dict)))
      (setq completion-at-point-functions (list #'cape-file
                                                (cape-super-capf #'cape-keyword
                                                                 #'cape-dabbrev
                                                                 #'citre-completion-at-point)))))

(use-package corfu
  :ensure t
  :demand t
  :bind (:map corfu-map
              ("RET" . nil)
              ("M-p" . corfu-popupinfo-scroll-down)
              ("M-n" . corfu-popupinfo-scroll-up))
  :after consult cape orderless marginalia vertico embark
  :hook (corfu-mode . corfu-popupinfo-mode)
  :init
  (setq corfu-auto t
        corfu-auto-prefix 1
        read-extended-command-predicate #'command-completion-default-include-p)
  :config
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (when completion-in-region--data
      (let ((completion-extra-properties corfu--extra)
            completion-cycle-threshold completion-cycling)
        (apply #'consult-completion-in-region completion-in-region--data))))
  (define-key corfu-map (kbd "C-c m") #'corfu-move-to-minibuffer)
  (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer)
  ;; Terminal specific settings for corfu
  (global-corfu-mode))
(use-package corfu-terminal
  :ensure t
  :demand t
  :after corfu
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))
(use-package svg-lib
  :ensure t)
(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
;;; corfu.el ends here
