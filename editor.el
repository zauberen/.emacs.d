;;; editor.el --- Configuration for text editor plugins -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; Citations and paper storage
(use-package citar
  :ensure t
  :no-require
  :after org
  :bind (:map org-mode-map
         :package org ("C-c i" . #'org-cite-insert))
  :config
  (if (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
      (setq org-cite-global-bibliography '("C:/org/org-notes/references.bib"))
      (setq org-cite-global-bibliography '("~/Documents/GitHub/org-notes/references.bib")))
  (setq citar-bibliography org-cite-global-bibliography)
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar))
(use-package citar-embark
  :after citar embark
  :ensure t
  :no-require
  :config (citar-embark-mode))
(use-package citar-denote
  :ensure t
  :after citar denote
  :config (citar-denote-mode))
(use-package bibtex
  :custom
  (bibtex-user-optional-fields
   '(("keywords" "Keywords to describe the entry" "")
     ("file"     "Relative or absolute path to attachments" "" )))
  (bibtex-align-at-equal-sign t))

;; Convert documents inside of emacs using pandoc
(use-package ox-pandoc
  :ensure t)

;; Spell checking
(use-package flycheck-vale
  :ensure t
  :after org
  :config
  (flycheck-vale-setup))

;; Elpaca fixed the dependency problems with this!
;; Useful dictionary/thesaurus program (requires internet)
(use-package powerthesaurus
  :ensure t
  :after (hydra evil)
  :config
  (evil-define-key 'normal 'global
    (kbd "SPC d") #'powerthesaurus-hydra/body)
  (evil-define-key 'visual 'global
    (kbd "SPC d") #'powerthesaurus-hydra/body))

;; Blogging platform integration
(use-package writefreely
  :ensure t
  :after org
  :config
  (setq writefreely-instance-url "https://write.as"
        writefreely-instance-api-endpoint "https://write.as/api"))
;;; editor.el ends here
