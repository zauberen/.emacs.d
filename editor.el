;;; editor.el --- Configuration for text editor plugins -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; Convert documents inside of emacs using pandoc
(use-package ox-pandoc
  :ensure t)

;; Basic vale integration
(defun vale-clear ()
  "Clears the vale buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*vale*") (erase-buffer)))
(defun vale-check-file ()
  "Starts tomcat on the configured tomcat-path."
  (interactive)
  (vale-clear)
  ;; Note that the double ampersand is a cross platform method to run 2 commands in 1 line
  (async-shell-command (concat "cd " default-directory " && vale " (buffer-file-name)) "*vale*"))

;; My function to delete empty lines
(defun flush-empty-lines ()
  "Remove empty lines in a file."
  (interactive)
  (flush-lines "^[[:space:]]*$"))

;; Useful dictionary/thesaurus program (requires internet)
(use-package powerthesaurus
  :ensure t
  :after (hydra evil)
  :config
  (evil-define-key 'normal 'global
    (kbd "SPC d") #'powerthesaurus-hydra/body)
  (evil-define-key 'visual 'global
    (kbd "SPC d") #'powerthesaurus-hydra/body))
(use-package dictionary
  :ensure nil
  :custom
  (dictionary-server "dict.org"))
;; This definition plugin supports offline dictionaries.
;; Need to set up a script to download them automatically.
(use-package define-word
  :ensure t
  :bind ("C-c d w" . define-word-at-point))

;; Blogging platform integration
(use-package writefreely
  :ensure t
  :after org
  :config
  (setq writefreely-instance-url "https://write.as"
        writefreely-instance-api-endpoint "https://write.as/api"))
;;; editor.el ends here
