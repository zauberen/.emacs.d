;;; corfu.el --- Packages for completion at point
;;; Commentary:
;;; cape, corfu, corfu-terminal
;;; Code:
;; Capf modifier
(use-package cape
  :ensure t
  :init
  (setq cape-dabbrev-min-length 2))
;; Terminal specific settings for corfu
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

;; Build all of the completion hooks
(use-package corfu
  :ensure t
  :demand t
  :bind (:map corfu-map
              ("RET" . nil)
              ("M-p" . corfu-popupinfo-scroll-down)
              ("M-n" . corfu-popupinfo-scroll-up))
  :after (cape orderless)
  :init
  (setq corfu-auto t
        corfu-auto-delay 0.3
        corfu-auto-prefix 2
        completion-cycle-threshold 5
        corfu-on-exact-match 'show)
  (when (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
      (setq text-mode-ispell-word-completion nil))
  ;;read-extended-command-predicate #'command-completion-default-include-p)
  :config
  (global-corfu-mode)
  :hook ((corfu-mode . corfu-popupinfo-mode)
         (conf-mode . (lambda ()
                        (if (eq system-type 'darwin)
                            (setq-local completion-at-point-functions
                                        (cons
                                         #'cape-file
                                         (cons
                                          (cape-capf-super
                                           #'yasnippet-capf
                                           ;;#'tempel-complete
                                           #'cape-keyword
                                           #'cape-dict)
                                          completion-at-point-functions)))
                          (setq-local completion-at-point-functions
                                      (cons
                                       #'cape-file
                                       (cons
                                        (cape-capf-super
                                         #'yasnippet-capf
                                         ;;#'tempel-complete
                                         #'cape-keyword
                                         #'cape-dict)
                                        completion-at-point-functions))))))
         (text-mode . (lambda ()
                        (if (eq system-type 'darwin)
                            (setq-local completion-at-point-functions
                                        (cons
                                         #'cape-file
                                         (cons
                                          (cape-capf-super
                                           ;;#'yasnippet-capf
                                           #'tempel-complete
                                           #'cape-keyword
                                           #'cape-dict)
                                          completion-at-point-functions)))
                          (setq-local completion-at-point-functions
                                      (cons
                                       #'cape-file
                                       (cons
                                        (cape-capf-super
                                         ;;#'yasnippet-capf
                                         #'tempel-complete
                                         #'cape-keyword
                                         #'cape-dict)
                                        completion-at-point-functions))))))
         (prog-mode . (lambda ()
                        (if (eq system-type 'darwin)
                            (setq-local completion-at-point-functions
                                        (append
                                         (list #'cape-file
                                               #'yasnippet-capf)
                                         completion-at-point-functions
                                         (list (cape-capf-super
                                                ;;#'yasnippet-capf
                                                ;;#'tempel-complete
                                                #'cape-keyword
                                                #'citre-completion-at-point
                                                #'cape-dabbrev
                                                #'cape-dict))))
                          (setq-local completion-at-point-functions
                                      (append
                                       (list #'cape-file
                                             #'yasnippet-capf)
                                       ;;#'tempel-complete)
                                       completion-at-point-functions
                                       (list (cape-capf-super
                                              ;;#'yasnippet-capf
                                              #'cape-keyword
                                              #'cape-dabbrev
                                              #'cape-dict)
                                             ;;#'citre-completion-at-point
                                             ))
                                      ))))
         (org-mode . (lambda ()
                       (if (eq system-type 'darwin)
                           (setq-local completion-at-point-functions
                                       (list #'cape-file
                                             (cape-capf-super
                                              #'yasnippet-capf
                                              #'tempel-complete
                                              #'cape-keyword
                                              #'cape-dabbrev
                                              #'cape-dict)))
                         (setq-local completion-at-point-functions
                                     (list #'cape-file
                                           (cape-capf-super
                                            #'yasnippet-capf
                                            #'tempel-complete
                                            #'cape-keyword
                                            #'cape-dict))))))))
;;; corfu.el ends here
