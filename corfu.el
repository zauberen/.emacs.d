;;; corfu.el --- Packages for completion at point -*- lexical-binding: t -*-
;;; Commentary:
;;; cape, corfu, corfu-terminal
;;; Code:
;; Capf modifier
(use-package cape
  :ensure t
  :hook ((conf-mode . (lambda () (my/build-capf conf-super-capf)))
         (text-mode . (lambda () (my/build-capf text-super-capf)))
         (prog-mode . (lambda () (if (eq system-type 'darwin)
                                     (my/build-capf macp-super-capf yasc-prece-capf t)
                                   (my/build-capf winp-super-capf yasc-prece-capf t))))
         (org-mode . (lambda () (my/build-capf org-super-capf))))
  :custom
  ;; Minad rewrote my method, this is the new "lisp only" setting
  (cape-dict-grep nil)
  ;; Maybe obsolete?
  (cape-dabbrev-min-length 2)
  ;; Dictionary is sourced from Ubuntu 22.04
  (cape-dict-file (concat org-directory "/wordlists/american-english"))
  ;; These are the defaults but I mess with them a lot
  (cape-dict-case-fold 'case-fold-search)
  (cape-dict-limit 100)
  :config
  (when (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
      (setq text-mode-ispell-word-completion nil))
  (setq completion-cycle-threshold 5
        conf-super-capf (cape-capf-super
                         #'yasnippet-capf
                         #'cape-keyword
                         #'cape-dict)
        text-super-capf (cape-capf-super
                         #'tempel-complete
                         #'cape-keyword
                         #'cape-dict)
        yasc-prece-capf (list #'cape-file
                              #'yasnippet-capf)
        winp-super-capf (cape-capf-super
                         #'cape-keyword
                         #'cape-dict)
        macp-super-capf (cape-capf-super
                         #'cape-keyword
                         #'citre-completion-at-point
                         #'cape-dict)
        org-super-capf (cape-capf-super
                        #'yasnippet-capf
                        #'tempel-complete
                        #'cape-keyword
                        #'cape-dict))
  (defun my/build-capf (super-capf &optional preceding-capf super-after)
    "Builds the completion-at-point-functions for a major mode.
SUPER-CAPF: The super capf to use.
PRECEDING-CAPF: The preceding capf list to use. By default this is cape-file.
SUPER-AFTER: If non-nil adds the super capf after completion-at-point-functions."
    (when (eq preceding-capf nil)
      (setq preceding-capf (list #'cape-file)))
    (if (not (eq super-after nil))
        (setq-local completion-at-point-functions
                    (append
                     preceding-capf
                     completion-at-point-functions
                     (list super-capf)))
      (setq-local completion-at-point-functions
                  (append
                   preceding-capf
                   (list super-capf)
                   completion-at-point-functions))))

  ;; Overwrite some functions in cape to pure elisp.
  (defvar cape--dictionary nil "Cached dictionary for cape-dict.")
  (defun cape--dict-load ()
    "Load or reload the dictionary wordlist."
    (interactive)
    (let ((files (mapcar #'expand-file-name
                         (ensure-list
                          (if (functionp cape-dict-file)
                              (funcall cape-dict-file)
                            cape-dict-file)))))
      (setq cape--dictionary
            (vconcat (split-string
                      (with-temp-buffer
                        (dolist (file files) (insert-file-contents file))
                        (buffer-string))
                      "\n")))))
  (defun cape--dict-list (input)
    "Return all words from `cape-dict-file' matching INPUT."
    (let* ((inhibit-message t)
           (message-log-max nil)
           (default-directory
            (if (and (not (file-remote-p default-directory))
                     (file-directory-p default-directory))
                default-directory
              user-emacs-directory))
           (words
            (let ((matches nil))
              (if (eq cape--dictionary nil)
                  (cape--dict-load))
              (catch 'maxed
                (seq-doseq (word cape--dictionary)
                  (if (length< matches cape-dict-limit)
                      (when (and (not (eq word nil))
                                 (string-match-p input word))
                        (setf matches (append matches (list word))))
                    (throw 'maxed matches))))
              matches)))
      (cons
       (apply-partially
        (if (and cape-dict-limit (length= words cape-dict-limit))
            #'equal #'string-search)
        input)
       (cape--case-replace-list cape-dict-case-replace input words)))))
  
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
        corfu-on-exact-match 'show)
  ;;read-extended-command-predicate #'command-completion-default-include-p)
  :config
  (global-corfu-mode)
  :hook (corfu-mode . corfu-popupinfo-mode))
;;; corfu.el ends here
