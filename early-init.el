;;; early-init.el --- Early init config -*- lexical-binding: t -*-
;;; Commentary:
;;; My early init
;;; Code:

;; Disable GC
(setq gc-cons-threshold most-positive-fixnum)

;; Hide UI elements
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars . 0) default-frame-alist)
(advice-add #'x-apply-session-resources :override #'ignore)
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      window-resize-pixelwise t)

;; Set up lsp-mode to use plists (for lsp-booster)
(setenv "LSP_USE_PLISTS" "true")

;; Disable package.el when using elpaca
(setq package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
