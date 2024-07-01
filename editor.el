;;; editor.el --- Configuration for text editor plugins
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

(use-package ox-pandoc
  :ensure t)

;; Does not work on unix, either this or some required dependency
;; No longer works in windows as well, new theory is that you must install this first, then vertico on top to make it work.
;; Last time I installed on windows, I commented out minibuffer.el, loaded, then uncommented and relaunched.
;; I don't want any weirdness like that required to use the system, so commenting out again
;; Useful dictionary/thesaurus program (requires internet)
;(if (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
    ;(use-package powerthesaurus
      ;:after hydra evil
      ;:config
      ;(evil-define-key 'normal 'global
        ;(kbd "SPC d") #'powerthesaurus-hydra/body)
      ;(evil-define-key 'visual 'global
        ;(kbd "SPC d") #'powerthesaurus-hydra/body)))
;;; editor.el ends here
