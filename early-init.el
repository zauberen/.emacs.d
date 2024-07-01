;;; early-init.el --- Early init config v20220829
;;; Commentary:
;;; Building basics still
;;; Code:

;; Disable GC
(setq gc-cons-threshold most-positive-fixnum)

;; Package vs elpaca switch
(setq use-elpaca t)

;; Hide UI elements
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
;;(push '(vertical-scroll-bars) default-frame-alist)
(advice-add #'x-apply-session-resources :override #'ignore)
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      window-resize-pixelwise t)

;; Disable package.el when using elpaca
(when (eq use-elpaca t)
  (setq package-enable-at-startup nil))

(provide 'early-init)
;;; early-init.el ends here
