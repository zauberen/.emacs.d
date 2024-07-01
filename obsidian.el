;;; obsidian.el --- Obsidian configuration
;;; Commentary:
;;; obsidian
;;; Code:
(use-package obsidian
  :ensure t
  :bind (("C-c o o" . obsidian-search)
         :map obsidian-mode-map
         ("C-c C-l" . obsidian-insert-wikilink)
         ("C-<return>" . obsidian-follow-link-at-point))
  :init
  (setq obsidian-inbox-directory "Inbox"
        obsidian-path (if (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
                          "C:/Users/dillona/Documents/projects/0notes/Notes/Notes"
                        "~/Documents/Obsidian/notes"))
  :config
  (obsidian-specify-path obsidian-path)
  (global-obsidian-mode t))
;;; obsidian.el ends here
