;;; corfu.el --- Packages for completion at point
;;; Commentary:
;;; cape, corfu, corfu-terminal
;;; Code:
(use-package cape
  :ensure t
  :pin melpa
  :hook ((conf-mode . (lambda ()
                        (if (eq system-type 'darwin)
                            (setq-local completion-at-point-functions
                                        (cons
                                         #'cape-file
                                         (cons
                                          (cape-capf-super
                                           #'yasnippet-capf
                                           ;#'tempel-complete
                                           #'cape-keyword
                                           #'cape-dabbrev
                                           #'cape-dict)
                                          completion-at-point-functions)))
                            (setq-local completion-at-point-functions
                                        (cons
                                         #'cape-file
                                         (cons
                                          (cape-capf-super
                                           #'yasnippet-capf
                                           ;#'tempel-complete
                                           #'cape-keyword
                                           #'cape-dabbrev)
                                          completion-at-point-functions))))))
         (text-mode . (lambda ()
                        (if (eq system-type 'darwin)
                            (setq-local completion-at-point-functions
                                        (cons
                                         #'cape-file
                                         (cons
                                          (cape-capf-super
                                           ;#'yasnippet-capf
                                           #'tempel-complete
                                           #'cape-keyword
                                           #'cape-dabbrev
                                           #'cape-dict)
                                          completion-at-point-functions)))
                            (setq-local completion-at-point-functions
                                        (cons
                                         #'cape-file
                                         (cons
                                          (cape-capf-super
                                           ;#'yasnippet-capf
                                           #'tempel-complete
                                           #'cape-keyword
                                           #'cape-dabbrev)
                                   completion-at-point-functions))))))
         (prog-mode . (lambda ()
                        (if (eq system-type 'darwin)
                            (setq-local completion-at-point-functions
                                        (append
                                         (list #'cape-file)
                                         completion-at-point-functions
                                         (list (cape-capf-super
                                                #'yasnippet-capf
                                                ;#'tempel-complete
                                                #'cape-keyword
                                                #'citre-completion-at-point
                                                #'cape-dabbrev
                                                #'cape-dict))))
                            (setq-local completion-at-point-functions
                                        (append
                                         (list #'cape-file
                                               #'yasnippet-capf)
                                               ;#'tempel-complete)
                                         completion-at-point-functions
                                         (list (cape-capf-super
                                           ;#'yasnippet-capf
                                           #'cape-keyword
                                           #'cape-dabbrev)
                                           ;#'citre-completion-at-point
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
                                              #'cape-dabbrev)))))))
  :init
  (setq cape-dabbrev-min-length 2)
  ;; (if (eq system-type 'darwin)
  ;;     (setq completion-at-point-functions
  ;;           (list #'cape-file
  ;;                 (cape-capf-super
  ;;                  ;#'tempel-complete
  ;;                  #'cape-keyword
  ;;                  #'cape-dabbrev
  ;;                  #'citre-completion-at-point
  ;;                  #'cape-dict)))
  ;;     (setq completion-at-point-functions
  ;;           (list #'cape-file
  ;;                 (cape-capf-super
  ;;                  ;#'tempel-complete
  ;;                  #'cape-keyword
  ;;                  #'cape-dabbrev
  ;;                  #'citre-completion-at-point))))
  )
;; VC doesn't work on windows for now
(use-package yasnippet-capf
  :ensure t
  :after cape)

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
        corfu-auto-delay 0
        corfu-auto-prefix 3
        completion-cycle-threshold 5)
        ;read-extended-command-predicate #'command-completion-default-include-p)
  :config
  (global-corfu-mode))
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
;;; corfu.el ends here
