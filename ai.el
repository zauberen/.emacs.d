;;; ai.el --- AI integration -*- lexical-binding: t -*-
;;; Commentary:
;;; AI plugins etc.
;;; Code:
(use-package gptel
  :ensure t
  :demand t
  :bind (("C-c C-a g" . gptel)
         ("C-c C-a a" . gptel-add-file)
         :map gptel-mode-map
         ("S-<return>" . gptel-send))
  :config
  ;; In local.el, call the function with an api key to use DeepSeek.
  ;; Here's an example:
  ;; (use-package gptel
  ;;   :ensure nil
  ;;   :config
  ;;   (setup-deep-seek "key"))
  (defun setup-deep-seek (apikey)
    "Sets up deepseek with the given APIKEY."
    (interactive)
    (setq gptel-model   'deepseek-reasoner
          gptel-backend (gptel-make-deepseek "DeepSeek"
                                             :stream t
                                             :key apikey))))
(use-package copilot-chat
  :ensure t
  :bind ("C-c C-a c" . copilot-chat-display)
  :config
  ; Fix doom-modeline
  (defun copilot-chat--org-goto-input()
    "Go to the input part of the chat buffer.
The input is created if not found."
    (goto-char (point-max))
    (let ((span (pm-innermost-span (point))))
      (if (and span
               (not (eq (car span) nil)))  ; nil span-type means host mode
          (goto-char (+ 1 (car (pm-innermost-range (point)))))
        (insert "\n\n")
        (let ((start (point))
              (inhibit-read-only t))
          (insert copilot-chat--org-delimiter "\n\n"))))))
;;; ai.el ends here
