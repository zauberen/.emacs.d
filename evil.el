;;; evil.el --- Basic evil configuration
;;; Commentary:
;;; evil-collection, evil, evil-mc, smartparens, evil-smartparens
;;; Code:
(use-package evil-collection
  :ensure t
  :pin melpa
  :demand t
  :diminish evil-collection-unimpaired-mode
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        ; Removes default insert bindings
        evil-disable-insert-state-bindings t)
  :config
  (evil-collection-init))
(use-package evil
  :ensure t
  :pin melpa
  :demand t
  :bind ("C-c C-s" . evil-avy-goto-char-timer)
  :after evil-collection avy
  :config
  ;; Allows redo functionality in evil
  ;; Only works in emacs 28 and later
  (evil-set-undo-system 'undo-redo)
  ;; This is the only insert binding I use, everything else is default emacs bindings
  (evil-define-key 'insert 'global
    (kbd "C-r") #'evil-paste-from-register)
  (evil-define-key 'normal 'global
    (kbd "SPC b") #'switch-to-buffer
    (kbd "SPC SPC") #'evil-avy-goto-word-or-subword-1
    (kbd "C-u") #'universal-argument
    (kbd "C-a") #'beginning-of-line
    (kbd "C-e") #'end-of-line
    (kbd "'") #'evil-goto-mark)
  (evil-define-key 'visual 'global
    (kbd "C-a") #'beginning-of-line
    (kbd "C-e") #'end-of-line
    ; Bound to a r since evil-org already has a default binding like this bound to a r
    (kbd "a r") #'mark-defun)
  (evil-mode 1))
(use-package evil-mc
  :ensure t
  :pin melpa
  :demand t
  :after evil
  :config
  (evil-define-local-var evil-mc-custom-paused nil
    "Paused functionality when there are multiple cursors active.")
  (defun evil-mc-pause-smartchr-for-mode (mode)
    "Temporarily disable the smartchr keys for MODE."
    (let ((m-mode (if (atom mode) mode (car mode)))
          (s-mode (if (atom mode) mode (cdr mode))))
      (let ((init (intern (concat "smartchr/init-" (symbol-name s-mode))))
            (undo (intern (concat "smartchr/undo-" (symbol-name s-mode)))))
        (when (eq major-mode m-mode)
          (funcall undo)
          (push `(lambda () (,init)) evil-mc-custom-paused)))))
  (defun evil-mc-before-cursors-setup-hook ()
    "Hook to run before any cursor is created.
Can be used to temporarily disable any functionality that doesn't
play well with `evil-mc'."
    (mapc 'evil-mc-pause-smartchr-for-mode
          '(web-mode js2-mode java-mode (enh-ruby-mode . ruby-mode) css-mode))
    (when (boundp 'whitespace-cleanup-disabled)
      (setq whitespace-cleanup-disabled t)
      (push (lambda () (setq whitespace-cleanup-disabled nil)) evil-mc-custom-paused)))
  (defun evil-mc-after-cursors-teardown-hook ()
    "Hook to run after all cursors are deleted."
    (dolist (fn evil-mc-custom-paused) (funcall fn))
    (setq evil-mc-custom-paused nil))
  (add-hook 'evil-mc-before-cursors-created 'evil-mc-before-cursors-setup-hook)
  (add-hook 'evil-mc-after-cursors-deleted 'evil-mc-after-cursors-teardown-hook)
  (global-evil-mc-mode 1)

  (evil-define-key 'normal 'global
    (kbd "SPC q") #'evil-mc-undo-all-cursors
    (kbd "SPC j") #'evil-mc-make-cursor-move-next-line
    (kbd "SPC k") #'evil-mc-make-cursor-move-prev-line)
  (evil-define-key 'visual evil-mc-key-map
    "A" #'evil-mc-make-cursor-in-visual-selection-end
    "I" #'evil-mc-make-cursor-in-visual-selection-beg))
;(defvar evil-mc-mode-line-prefix "mc"
  ;"Override of the default mode line string for `evil-mc-mode'.")

;; Smart parens (select and use keybinds to place parens)
(use-package smartparens
  :ensure t
  :pin melpa
  :demand t
  :bind (("C-c (" . sp-wrap-round)
         ("C-c {" . sp-wrap-curly)
         ("C-c [" . sp-wrap-square)
         ("C-c u" . sp-unwrap-sexp)
         ("C-c w w" . sp-rewrap-sexp)
         ("C-c w r" . sp-wrap-round)
         ("C-c w c" . sp-wrap-curly)
         ("C-c w s" . sp-wrap-square)
         ("C-c w u" . sp-unwrap-sexp))
  :config
  (defun sp-wrap-quote ()
    "Wrap text with a single quote"
    (interactive)
    (sp-wrap-with-pair "'"))
  (defun sp-wrap-double-quote ()
    "Wrap text with a double quote"
    (interactive)
    (sp-wrap-with-pair "\""))
  (define-key global-map (kbd "C-c \"") #'sp-wrap-double-quote)
  (define-key global-map (kbd "C-c '") #'sp-wrap-quote)
  (define-key global-map (kbd "C-c w d") #'sp-wrap-double-quote)
  (define-key global-map (kbd "C-c w q") #'sp-wrap-quote))
(use-package evil-smartparens
  :ensure t
  :pin melpa
  :demand t
  :after smartparens evil
  :hook (smartparens-enabled . evil-smartparens-mode)
  :config
  (smartparens-global-mode 1)
  (smartparens-strict-mode 1))
;;; evil.el ends here
