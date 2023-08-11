;;; org.el --- Org and Denote configuration
;;; Commentary:
;;; org-contrib, org, denote, evil-org, calendar, hl-todo, org-journal, org-super-agenda
;;; Code:
(use-package org-contrib
  :ensure t
  :demand t)
(use-package org
  :ensure t
  :after evil evil-org org-contrib consult
  :demand t
  :diminish org-indent-mode eldoc-mode auto-revert-mode
  ; Enable word wrap and org indenting
  :hook ((org-mode . toggle-truncate-lines)
         (org-mode . org-indent-mode))
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map org-mode-map
         ; Ensure consult gets it's bindings in org
         ("C-," . consult-yank-from-kill-ring)
         ("C-'" . consult-bookmark)
         ; Custom org bindings
         ("C-c ," . org-time-stamp-inactive)
         ("C-c ." . org-time-stamp)
         ("C-c x" . org-cut-subtree)
         ("S-<return>" . evil-org-open-below))
  :mode (("\\.org\\'" . org-mode)
         ("\\.org$" . org-mode))
  :init
  ;; Set org directory
  (if (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
      (setq org-directory '"C:/org/org-notes")
      (setq org-directory '"~/Documents/GitHub/org-notes"))
  (setq org-enforce-todo-dependencies t
        org-enfocre-todo-checkbox-dependencies t
        diary-file (concat org-directory "/diary")
        appt-display-diary nil
        ;; Archive when closed
        org-todo-state-tags-triggers '(("CLOSED" ("ARCHIVE" . t)))
        ;; Theming
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-tags-column 0
        org-auto-align-tags nil
        org-agenda-tags-column 0
        org-catch-invisible-edits 'show-and-error
        org-special-ctrl-a/e t
        org-insert-heading-respect-content t
        ;; Follow links on RET (cannot always use gx)
        org-return-follows-link t
        ;; Make org look better
        org-hide-leading-stars t
        ;; When a todo is set to done, add the completion time
        org-log-done 'note
        ;; Include diary stuff in the org agenda
        org-agenda-include-diary t
        ;; Set default directories, files
        org-default-notes-file (concat org-directory "/inbox.org")
        org-work-file (concat org-directory "/org-private/work.org")
        org-home-file (concat org-directory "/home.org")
        org-app-file (concat org-directory "/org-private/dnd-app.org")
        org-agenda-files (seq-filter
                          (lambda(x)
                            (and (not (string-match "/archive/"(file-name-directory x)))
                                 (not (string-match "/denote/"(file-name-directory x)))
                                 (not (string-match "/journal/"(file-name-directory x)))))
                          (directory-files-recursively org-directory "\\.org$"))
        org-archive-location (concat "%s_archive::" org-directory "/archive")
        org-tag-persistent-alist '((:startgroup . nil)
                                   ("important" . ?i)
                                   ("backlog" . ?x)
                                   (:endgroup . nil)
                                   ("ARCHIVE" . ?A)
                                   (:startgroup . nil)
                                   ("@Work" . ?w)
                                   ("@Home" . ?h)
                                   ("@Personal" . ?p)
                                   (:endgroup . nil))
        org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CLOSED"))
        org-capture-templates '(("t" "Inbox" entry (file+headline org-default-notes-file "Inbox")
                                 "* TODO %?\n:PROPERTIES:\n:CREATION: %U\n:END:\n")
                                ("w" "Work Task" entry (file+headline org-work-file "Inbox")
                                 "* TODO %?\n:PROPERTIES:\n:CREATION: %U\n:TRACKZILLA: N/A\n:END:\n")
                                ("i" "Work Issue" entry (file+headline org-work-file "Inbox")
                                 "* TODO %?\n:PROPERTIES:\n:CREATION: %U\n:TRACKZILLA: N/A\n:END:\n#+TITLE: Issue comment\n#+BEGIN_SRC markdown :hidden\n#+END_SRC\n")
                                ("h" "Home Task" entry (file+headline org-home-file "Inbox")
                                 "* TODO %?\n:PROPERTIES:\n:CREATION: %U\n:END:\n")
                                ("a" "App Task" entry (file+headline org-app-file "Inbox")
                                 "* TODO %?\n:PROPERTIES:\n:CREATION: %U\n:END:\n")
                                ("j" "Journal" entry (file+datetree org-default-notes-file)
                                 "* %?\n%U\n%a")
                                ("n" "Link Inbox" entry (file+headline org-default-notes-file "Inbox")
                                 "* TODO %?\n:PROPERTIES:\n:CREATION: %U\n:END:\n%a")
                                ("o" "Link Work Issue" entry (file+headline org-work-file "Inbox")
                                 "* TODO %?\n:PROPERTIES:\n:CREATION: %U\n:END:\n%a")
                                ("m" "Link Home Task" entry (file+headline org-home-file "Tasks")
                                 "* TODO %?\n:PROPERTIES:\n:CREATION: %U\n:END:\n%a")))
  :config
  ; Shamelessly stolen from https://emacs.stackexchange.com/questions/44914/choose-individual-startup-visibility-of-org-modes-source-blocks
  ; This code lets you put :hidden on an org code block to hide it by default
  (defun individual-visibility-source-blocks ()
    "Fold some blocks in the current buffer."
    (interactive)
    (org-show-block-all)
    (org-block-map
     (lambda ()
       (let ((case-fold-search t))
         (when (and
                (save-excursion
                  (beginning-of-line 1)
                  (looking-at org-block-regexp))
                (cl-assoc
                 ':hidden
                 (cl-third
                  (org-babel-get-src-block-info))))
           (org-hide-block-toggle))))))
  (add-hook 'org-mode-hook (function individual-visibility-source-blocks))
  (appt-activate 1)
  (require 'org-checklist))

; Denote settings
(use-package denote
  :ensure t
  :after evil org
  :demand t
  ; Make denote links work
  :hook ((find-file . denote-link-buttonize-buffer)
         ; Denote formatting for files in dired
         (dired-mode . denote-dired-mode))
  :bind (("C-c n n" . denote)
         ("C-c n r" . denote-rename-file)
         ("C-c n l" . denote-link)
         ("C-c n t" . denote-type)
         ("C-c n f" . denote-rename-file-using-front-matter))
  :init
  ; Copied from org settings
  (if (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
      (setq org-directory '"C:/org/org-notes")
      (setq org-directory '"~/Documents/GitHub/org-notes"))
  (setq denote-directory (concat org-directory "/denote")
        ; Eventually I want template added here, need to define denote-templates
        denote-prompts '(subdirectory title keywords)
        denote-file-type 'org
        denote-known-keywords '("emacs" "work" "reflection" "denote" "politics" "philosophy" "recipe" "discussion")
        denote-date-prompt-use-org-read-date t)
  :config
  (evil-define-key 'normal 'global
    (kbd "SPC n n") 'denote
    (kbd "SPC n r") 'denote-rename-file
    (kbd "SPC n l") 'denote-link
    (kbd "SPC n t") 'denote-type
    (kbd "SPC n f") 'denote-rename-file-using-front-matter))
;; Diary and calendar
(use-package calendar
  :ensure t
  :after org
  :bind (("C-x c" . calendar)
         ("C-x y" . diary))
  :hook ((today-visible-calendar . calendar-mark-today)
         (diary-display . fancy-diary-display)
         (list-diary-entries . sort-diary-entries)
         (list-diary-entries . include-other-diary-files)
         (mark-diary-entries . mark-include-diary-files))
  :init
  (setq view-diary-entries-initially t
        appt-display-mode-line t
        view-calendar-holidays-initially t
        mark-diary-entries-in-calendar t
        mark-holidays-in-calendar t))

;; TODO keywords in org
(use-package hl-todo
  :ensure t
  :pin melpa
  :init
  (setq hl-todo-keyword-faces
	    '(("TODO"           . "#FF0000")
	      ("IN-PROGRESS"    . "#A020F0")
	      ("WAITING"        . "#FF4500")
	      ("DONE"           . "#1E90FF")
          ("CLOSED"         . "#1E90FF")))
  :config
  (global-hl-todo-mode))
; Load org extensions
(use-package org-journal
  :ensure t
  :pin melpa
  :after org
  :bind ("C-c j j" . org-journal-new-entry)
  :init
  (setq org-journal-dir (concat org-directory "/journal")
        org-journal-prefix-key "C-c j "
        org-journal-enable-agenda-integration t
        org-journal-file-format "%Y-%m.org"
        org-journal-file-type 'monthly))
(use-package org-super-agenda
  :ensure t
  :pin melpa
  :demand t
  :after org
  :init
  (setq org-super-agenda-groups
        '((:name "Personal Tasks"
                 :tag "@Home"
                 :tag "@Runescape"
                 :tag "@Personal"
                 :order 3)
          (:name "Schedule"
                 :time-grid t
                 :todo "IN-PROGRESS"
                 :order 0)
          (:name "Important"
                 :tag "important"
                 :priority "A"
                 :order 1)
          (:name "High-Priority"
                 :priority "B"
                 :order 2)
          (:name "Medium-Priority"
                 :priority "C"
                 :order 4)))
  :config
  (org-super-agenda-mode))
(use-package evil-org
  :ensure t
  :pin melpa
  :demand t
  :diminish evil-org-mode
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
  (evil-org-agenda-set-keys))
;;; org.el ends here
